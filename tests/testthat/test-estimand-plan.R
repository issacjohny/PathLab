pl_plan_fixture<-function(name,general=FALSE) {
    root<-getOption("pathlab.root")
    folder<-if(!is.null(root))file.path(root,"modules/pathlab/tests/fixtures") else testthat::test_path("..","fixtures")
    if(general)folder<-file.path(folder,"native-general")
    q<-jsonlite::fromJSON(file.path(folder,paste0(name,"-request.json")),simplifyVector=FALSE)
    d<-read.csv(file.path(folder,paste0(name,".csv")),check.names=FALSE)
    model<-validate_spec(q$graph,q$settings,names(d));sample<-prepare_sample(d,model)
    equations<-fit_equations(compile_equations(model,sample),FALSE)
    indices<-as.matrix(read.csv(file.path(folder,paste0(name,"-indices.csv")),header=FALSE))
    list(model=model,sample=sample,equations=equations,indices=indices)
}
pl_plan_values<-function(rows)vapply(rows,`[[`,0.0,"estimate")
pl_plan_beta<-function(f,draw)lapply(f$equations,function(e)pl_fit(e$design[f$indices[draw,],,drop=FALSE],e$y[f$indices[draw,]],coefficients_only=TRUE))

testthat::test_that("compiled estimands preserve every row and arithmetic for larger saved-index models",{
    for(name in c("joint3_two","separate3_three")) {
        f<-pl_plan_fixture(name,TRUE)
        original<-evaluate_estimands(f$model,f$sample,f$equations)
        plan<-pl_compile_estimand_plan(f$model,f$sample,f$equations,original)
        testthat::expect_length(plan$rows,length(original))
        testthat::expect_identical(pl_evaluate_estimand_plan(plan,lapply(f$equations,function(e)e$fit$coefficients)),pl_plan_values(original))
        for(draw in c(1L,37L,200L)) {
            betas<-pl_plan_beta(f,draw)
            oracle<-evaluate_estimands(f$model,f$sample,f$equations,betas)
            testthat::expect_identical(vapply(oracle,`[[`,"","id"),vapply(original,`[[`,"","id"))
            testthat::expect_identical(pl_evaluate_estimand_plan(plan,betas),pl_plan_values(oracle))
        }
    }
})

testthat::test_that("compiled cubic repeated-W and constant indices preserve every paired distribution",{
    f<-pl_plan_fixture("repeated_w")
    for(constant in c(FALSE,TRUE)) {
        model<-f$model
        if(constant) {
            graph<-model$spec;graph$moderations<-graph$moderations[1]
            model<-validate_spec(graph,model$settings,vapply(model$by,`[[`,"","column"))
        }
        equations<-fit_equations(compile_equations(model,f$sample),FALSE)
        original<-evaluate_estimands(model,f$sample,equations)
        plan<-pl_compile_estimand_plan(model,f$sample,equations,original)
        testthat::expect_gt(sum(vapply(original,function(r)r$kind=="linear_index",TRUE)),0)
        if(!constant)testthat::expect_gt(sum(vapply(original,function(r)r$kind=="conditional_derivative",TRUE)),0)
        expected<-actual<-matrix(0,200,length(original))
        for(draw in 1:200) {
            betas<-lapply(equations,function(e)pl_fit(e$design[f$indices[draw,],,drop=FALSE],e$y[f$indices[draw,]],coefficients_only=TRUE))
            expected[draw,]<-pl_plan_values(evaluate_estimands(model,f$sample,equations,betas))
            actual[draw,]<-pl_evaluate_estimand_plan(plan,betas)
        }
        testthat::expect_identical(actual,expected)
        result<-bootstrap_estimands(model,f$sample,equations,200L,419L,f$indices)
        testthat::expect_identical(result$distribution,expected)
        testthat::expect_identical(result$accepted_draws,1:200)
        testthat::expect_identical(vapply(result$rows,`[[`,"","id"),vapply(original,`[[`,"","id"))
        testthat::expect_identical(result$metadata$failed,0L)
        for(k in seq_along(original)) {
            testthat::expect_identical(result$rows[[k]]$bootstrap_se,stats::sd(expected[,k]))
            testthat::expect_identical(c(result$rows[[k]]$lower,result$rows[[k]]$upper),unname(stats::quantile(expected[,k],c(.025,.975),type=7)))
        }
    }
})

testthat::test_that("compiled estimands preserve special labels, omitted direct paths and index keys",{
    for(name in c("missing_special","first_stage","mediation","ordinary_moderation")) {
        f<-pl_plan_fixture(name)
        original<-evaluate_estimands(f$model,f$sample,f$equations)
        plan<-pl_compile_estimand_plan(f$model,f$sample,f$equations,original)
        for(draw in c(1L,200L)) {
            betas<-pl_plan_beta(f,draw)
            testthat::expect_identical(pl_evaluate_estimand_plan(plan,betas),pl_plan_values(evaluate_estimands(f$model,f$sample,f$equations,betas)))
        }
    }
    f<-pl_plan_fixture("first_stage")
    graph<-f$model$spec
    graph$edges<-Filter(function(e)!(e$source==f$model$x && e$target==f$model$y),graph$edges)
    model<-validate_spec(graph,f$model$settings,vapply(f$model$by,`[[`,"","column"))
    equations<-fit_equations(compile_equations(model,f$sample),FALSE)
    original<-evaluate_estimands(model,f$sample,equations)
    plan<-pl_compile_estimand_plan(model,f$sample,equations,original)
    testthat::expect_true(all(vapply(Filter(function(r)r$kind=="direct",original),function(r)identical(r$estimate,0),TRUE)))
    testthat::expect_identical(pl_evaluate_estimand_plan(plan,lapply(equations,function(e)e$fit$coefficients)),pl_plan_values(original))
    # Keep equivalent unrounded probe conditions distinct even at near-machine
    # spacing. Compile keys use the condition's integer slot, never formatted values.
    f<-pl_plan_fixture("first_stage")
    f$sample$conditions<-list(list(w=1),list(w=1+.Machine$double.eps*4))
    original<-evaluate_estimands(f$model,f$sample,f$equations)
    plan<-pl_compile_estimand_plan(f$model,f$sample,f$equations,original)
    testthat::expect_identical(pl_evaluate_estimand_plan(plan,lapply(f$equations,function(e)e$fit$coefficients)),pl_plan_values(original))
    bad<-f$indices;bad[1,1]<-0
    testthat::expect_error(bootstrap_estimands(f$model,f$sample,f$equations,200L,419L,bad),"INVALID_OPTIONS")
})

testthat::test_that("compiled derivative cache keys preserve adversarial underscore identifiers",{
    f<-pl_plan_fixture("separate_three")
    replacements<-c(q="a_b",w="a",z="b")
    rename<-function(id)if(id %in% names(replacements))unname(replacements[id]) else id
    graph<-f$model$spec
    graph$nodes<-lapply(graph$nodes,function(n){n$id<-rename(n$id);n})
    graph$edges<-lapply(graph$edges,function(e){e$source<-rename(e$source);e$target<-rename(e$target);e})
    graph$moderations<-lapply(graph$moderations,function(m){m$moderator_ids<-lapply(m$moderator_ids,rename);m})
    settings<-f$model$settings
    settings$probes<-lapply(settings$probes,function(p){p$node_id<-rename(p$node_id);p})
    model<-validate_spec(graph,settings,vapply(f$model$by,`[[`,"","column"))
    sample<-f$sample
    names(sample$values)<-vapply(names(sample$values),rename,"")
    names(sample$probes)<-vapply(names(sample$probes),rename,"")
    sample$conditions<-lapply(sample$conditions,function(c){names(c)<-vapply(names(c),rename,"");c})
    equations<-fit_equations(compile_equations(model,sample),FALSE)
    original<-evaluate_estimands(model,sample,equations)
    plan<-pl_compile_estimand_plan(model,sample,equations,original)
    testthat::expect_identical(pl_evaluate_estimand_plan(plan,lapply(equations,function(e)e$fit$coefficients)),pl_plan_values(original))
    testthat::expect_false(anyDuplicated(vapply(original,`[[`,"","id"))>0)
})
