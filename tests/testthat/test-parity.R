root<-getOption("pathlab.root")
fixture<-if(!is.null(root))file.path(root,"modules/pathlab/tests/fixtures") else testthat::test_path("..","fixtures")
readj<-function(p)jsonlite::fromJSON(p,simplifyVector=FALSE)
manifest<-readj(file.path(fixture,"manifest.json"))
condition_key<-function(c)if(length(c))paste(sort(paste(names(c),sprintf("%.10g",unlist(c)),sep="=")),collapse=";") else ""
effect_key<-function(r)paste(r$kind,paste(sort(unlist(r$path_ids)),collapse=","),condition_key(r$condition),sep="|")

for(entry in manifest) for(robust in c(FALSE,TRUE)) local({
    case<-entry$name; hc3<-robust
    testthat::test_that(paste(case,if(hc3)"HC3" else "classical","matches saved-index Python and independent lm algebra"),{
        request<-readj(file.path(fixture,paste0(case,"-request.json")))
        d<-read.csv(file.path(fixture,paste0(case,".csv")),check.names=FALSE)
        indices<-as.matrix(read.csv(file.path(fixture,paste0(case,"-indices.csv")),header=FALSE))
        r<-pathlab_fit(d,request$graph,request$settings,draws=200,seed=419,robust=hc3,indices=indices)
        py<-readj(file.path(fixture,paste0(case,"-python.json")))[[tolower(as.character(hc3))]]
        # compute() returns its native process object plus engine transport fields.
        if(is.null(py$equations) && !is.null(py$diagnostics))py<-py$diagnostics$process
        testthat::expect_gt(length(py$equations),0)
        testthat::expect_gt(length(py$effects),0)
        testthat::expect_length(r$equations,length(py$equations))
        testthat::expect_equal(r$sample$n,entry$n)
        testthat::expect_equal(r$bootstrap$successful,200)
        testthat::expect_equal(r$bootstrap$failed,0)
        for(pe in py$equations) {
            e<-r$equations[[pe$target_id]]; f<-e$fit
            actual_names<-vapply(e$terms,function(t)paste(t,collapse=":"),"")
            python_names<-vapply(pe$coefficients,function(row)paste(vapply(row$factors,`[[`,"","node_id"),collapse=":"),"")
            testthat::expect_equal(actual_names,python_names,ignore_attr=TRUE)
            testthat::expect_equal(unname(f$coefficients),vapply(pe$coefficients,`[[`,0.0,"estimate"),tolerance=1e-8)
            pcov<-do.call(rbind,lapply(pe$covariance$values,unlist))
            testthat::expect_equal(unname(f$covariance),unname(pcov),tolerance=1e-8)
            for(stat in c("se","p","lower","upper"))testthat::expect_equal(unname(f[[stat]]),vapply(pe$coefficients,`[[`,0.0,stat),tolerance=1e-8)
            # Independent raw data and handwritten formula, bypassing compiler.
            raw<-d[complete.cases(d),,drop=FALSE]
            names(raw)<-vapply(request$graph$nodes,function(n)n$id,"")[match(names(raw),vapply(request$graph$nodes,`[[`,"","column"))]
            reference_terms<-unlist(request$reference_equations[[pe$target_id]])
            independent<-stats::lm(as.formula(paste(pe$target_id,"~",paste(reference_terms,collapse="+"))),raw)
            testthat::expect_equal(dim(e$design),dim(model.matrix(independent)))
            testthat::expect_equal(as.vector(e$design),as.vector(model.matrix(independent)),tolerance=1e-12)
            testthat::expect_equal(unname(f$coefficients),unname(coef(independent)),tolerance=1e-10)
            X<-model.matrix(independent); bread<-solve(crossprod(X)); residual<-resid(independent)
            cov<-if(hc3) {h<-rowSums((X%*%bread)*X); bread%*%crossprod(X,X*(residual/(1-h))^2)%*%bread} else vcov(independent)
            testthat::expect_equal(unname(f$covariance),unname(cov),tolerance=1e-10)
            testthat::expect_equal(f$df_residual,df.residual(independent))
            testthat::expect_equal(f$r_squared,summary(independent)$r.squared,tolerance=1e-12)
        }
        # The public effect fields have different internal IDs; match semantics.
        for(p in py$effects) {
            candidates<-Filter(function(a)a$kind==p$kind && condition_key(a$condition)==condition_key(p$condition) && (a$kind!="specific_indirect" || setequal(a$path_ids,unlist(p$path_ids))),r$estimands)
            testthat::expect_length(candidates,1)
            if(length(candidates)==1)for(stat in c("estimate","bootstrap_se","lower","upper"))testthat::expect_equal(candidates[[1]][[stat]],p[[stat]],tolerance=1e-8)
        }
        for(p in py$indices) {
            candidates<-Filter(function(a)a$kind==p$kind && a$effect_kind==p$effect_kind && setequal(a$moderator_ids,unlist(p$moderator_ids)) && condition_key(a$condition)==condition_key(p$condition) && (a$effect_kind!="specific_indirect" || setequal(a$path_ids,unlist(p$path_ids))),r$estimands)
            testthat::expect_length(candidates,1)
            if(length(candidates)==1)for(stat in c("estimate","bootstrap_se","lower","upper"))testthat::expect_equal(candidates[[1]][[stat]],p[[stat]],tolerance=1e-8)
        }
        # Compare every paired effect/index contrast, with the same joint draws.
        testthat::expect_equal(sum(vapply(r$estimands,function(v)v$kind=="route_derivative_difference",TRUE)),length(py$index_contrasts))
        testthat::expect_equal(sum(vapply(r$estimands,function(v)v$kind %in% c("route_difference","condition_difference"),TRUE)),length(py$effect_contrasts))
        testthat::expect_false(anyDuplicated(vapply(r$estimands,`[[`,"","id"))>0)
        semantic_key<-function(v,isindex)paste(v$kind,if(isindex)v$effect_kind else "",if(isindex)paste(sort(unlist(v$moderator_ids)),collapse=",") else "",condition_key(v$condition),if((if(isindex)v$effect_kind else v$kind)=="specific_indirect")paste(sort(unlist(v$path_ids)),collapse=",") else "",sep="|")
        sources<-list(
          effect_contrasts=Filter(function(v)v$kind %in% c("specific_indirect","total_indirect","direct","total"),r$estimands),
          index_contrasts=Filter(function(v)v$kind %in% c("linear_index","conditional_index","conditional_derivative","mixed_index","conditional_mixed_derivative"),r$estimands))
        for(group in names(sources))sources[[group]]<-setNames(sources[[group]],vapply(sources[[group]],semantic_key,"",isindex=group=="index_contrasts"))
        contrast_rows<-Filter(function(v)!is.null(v$effect_a),r$estimands)
        contrast_key<-function(kind,a,b)paste(kind,a,b,sep="|")
        contrast_lookup<-setNames(contrast_rows,vapply(contrast_rows,function(v)contrast_key(v$kind,v$effect_a,v$effect_b),""))
        for(group in c("effect_contrasts","index_contrasts")) for(p in py[[group]]) {
            isindex<-group=="index_contrasts"
            source<-if(isindex)py$indices else py$effects
            idfield<-if(isindex)c("index_a","index_b") else c("effect_a","effect_b")
            find<-function(id) {
                pp<-Filter(function(v)v$id==id,source)[[1]]
                sources[[group]][[semantic_key(pp,isindex)]]
            }
            a<-find(p[[idfield[1]]]); b<-find(p[[idfield[2]]])
            expected_kind<-if(isindex)"route_derivative_difference" else if(p$kind=="specific_indirect_route_difference")"route_difference" else p$kind
            # Every reference contrast must actually be published by the R
            # engine, with source IDs preserved. Condition enumeration order
            # differs between languages, so accept an explicitly reversed
            # pair only with its estimate and CI endpoints reversed too.
            emitted<-Filter(Negate(is.null),list(contrast_lookup[[contrast_key(expected_kind,a$id,b$id)]],contrast_lookup[[contrast_key(expected_kind,b$id,a$id)]]))
            testthat::expect_length(emitted,1)
            if(length(emitted)==1) {
                rr<-emitted[[1]]; forward<-identical(rr$effect_a,a$id)
                testthat::expect_identical(rr$id,paste("contrast",rr$effect_a,rr$effect_b,sep="_"))
                testthat::expect_equal(rr$estimate,if(forward)p$estimate else -p$estimate,tolerance=1e-8)
                testthat::expect_equal(rr$bootstrap_se,p$bootstrap_se,tolerance=1e-8)
                testthat::expect_equal(c(rr$lower,rr$upper),if(forward)c(p$lower,p$upper) else -c(p$upper,p$lower),tolerance=1e-8)
            }
            testthat::expect_equal(a$estimate-b$estimate,p$estimate,tolerance=1e-8)
            ia<-match(a$id,vapply(r$estimands,`[[`,"","id")); ib<-match(b$id,vapply(r$estimands,`[[`,"","id"))
            difference<-r$bootstrap_distribution[,ia]-r$bootstrap_distribution[,ib]
            testthat::expect_equal(sd(difference),p$bootstrap_se,tolerance=1e-8)
            testthat::expect_equal(unname(quantile(difference,c(.025,.975),type=7)),c(p$lower,p$upper),tolerance=1e-8)
        }
        if(case=="first_stage" && !hc3 && !is.null(root) && isTRUE(getOption("pathlab.refresh_example",FALSE))) jsonlite::write_json(pl_public_result(r),file.path(root,"modules/pathlab/inst/examples/serial-first-stage-result.json"),auto_unbox=TRUE,pretty=TRUE,digits=16)
    })
})
