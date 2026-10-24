pl_resource_categorical_example<-function() {
    grid<-expand.grid(x=1:8,w=1:8)
    data<-grid[rep(seq_len(nrow(grid)),length.out=200),,drop=FALSE]
    data$y<-rep(c(0,1),100)
    graph<-list(schema_version=1,nodes=list(
        list(id="x",column="x",role="independent",measurement_scale="nominal"),
        list(id="w",column="w",role="moderator",measurement_scale="nominal"),
        list(id="y",column="y",role="dependent",measurement_scale="nominal")),
        edges=list(list(id="p_x_y",source="x",target="y"),list(id="p_w_y",source="w",target="y")),
        moderations=list(list(id="mod_x_y",path_id="p_x_y",moderator_ids=list("w"))))
    settings<-list(outcome_family="logistic",categorical=list(list(node_id="x",levels=1:8,reference=1),list(node_id="w",levels=1:8,reference=1)),probes=list())
    list(data=data,graph=graph,settings=settings)
}

testthat::test_that("over-budget logistic graphs are rejected before any original equation fit",{
    example<-pl_resource_categorical_example()
    called<-FALSE
    sandbox<-new.env(parent=environment(pathlab_fit))
    sandbox$fit_equations<-function(...) { called<<-TRUE;stop("Original equation fitter was reached") }
    fit<-pathlab_fit;environment(fit)<-sandbox
    testthat::expect_error(fit(example$data,example$graph,example$settings,draws=5000),"RESOURCE_LIMIT")
    testthat::expect_false(called)
    model<-validate_spec(example$graph,example$settings,names(example$data))
    sample<-prepare_sample(example$data,model);equations<-compile_equations(model,sample)
    testthat::expect_equal(ncol(equations$y$design),64)
    testthat::expect_error(pl_check_work(model,sample,equations,1000L),"RESOURCE_LIMIT")
    equations$y$family<-"linear"
    testthat::expect_true(pl_check_work(model,sample,equations,1000L))
})

testthat::test_that("combined design-cell guard runs before allocating even one equation matrix",{
    example<-pl_resource_categorical_example()
    model<-validate_spec(example$graph,example$settings,names(example$data))
    # Deliberately omit values: a guard placed after X construction would reach
    # the intercepted allocation instead of returning RESOURCE_LIMIT.
    sample<-list(n=2e8,basis=list(x=paste0("x@",1:7),w=paste0("w@",1:7)),values=list())
    allocated<-FALSE
    sandbox<-new.env(parent=environment(compile_equations))
    sandbox$vapply<-function(...) { allocated<<-TRUE;stop("An equation matrix allocation was reached") }
    compile<-compile_equations;environment(compile)<-sandbox
    testthat::expect_error(compile(model,sample),"RESOURCE_LIMIT")
    testthat::expect_false(allocated)
})

testthat::test_that("ordinary legacy continuous models retain a cheap valid work preflight",{
    fixture<-if(!is.null(getOption("pathlab.root")))file.path(getOption("pathlab.root"),"modules/pathlab/tests/fixtures") else testthat::test_path("..","fixtures")
    request<-jsonlite::fromJSON(file.path(fixture,"first_stage-request.json"),simplifyVector=FALSE)
    data<-read.csv(file.path(fixture,"first_stage.csv"),check.names=FALSE)
    model<-validate_spec(request$graph,request$settings,names(data));sample<-prepare_sample(data,model)
    compiled<-compile_equations(model,sample)
    testthat::expect_true(pl_check_work(model,sample,compiled,200L))
    testthat::expect_true(pl_check_work(model,sample,compiled,5000L))
    testthat::expect_true(all(vapply(compiled,function(e)identical(e$family,"linear"),TRUE)))
})
