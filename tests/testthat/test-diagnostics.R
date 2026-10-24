fixture<-if(!is.null(getOption("pathlab.root")))file.path(getOption("pathlab.root"),"modules/pathlab/tests/fixtures") else testthat::test_path("..","fixtures")
for(case in c("ordinary_moderation","first_stage","repeated_w"))for(hc3 in c(FALSE,TRUE))local({
    cc<-case; robust<-hc3
    testthat::test_that(paste(cc,robust,"model/block tests, slopes and in-range JN boundaries match independent references"),{
        q<-jsonlite::fromJSON(file.path(fixture,paste0(cc,"-request.json")),simplifyVector=FALSE)
        py<-jsonlite::fromJSON(file.path(fixture,paste0(cc,"-python.json")),simplifyVector=FALSE)[[tolower(as.character(robust))]]
        d<-read.csv(file.path(fixture,paste0(cc,".csv")))
        m<-validate_spec(q$graph,q$settings,names(d)); s<-prepare_sample(d,m); eq<-fit_equations(compile_equations(m,s),robust)
        slopes<-pl_slopes(m,s,eq,.05)
        for(pe in py$equations) {
            f<-eq[[pe$target_id]]$fit
            testthat::expect_equal(f$model_test$statistic,pe$overall_test$statistic,tolerance=1e-8)
            testthat::expect_equal(f$model_test$p,pe$overall_test$p,tolerance=1e-8)
            testthat::expect_equal(f$df_residual,pe$df_resid)
        }
        for(p in py$slopes) {
            rows<-Filter(function(r)r$path_id==p$path_id && abs(r$condition$w-p$condition$w)<1e-12,slopes$slopes)
            testthat::expect_length(rows,1)
            for(stat in c("estimate","se","lower","upper","p"))testthat::expect_equal(rows[[1]][[stat]],p[[stat]],tolerance=1e-8)
        }
        for(p in py$johnson_neyman)if(p$status=="computed") {
            r<-Filter(function(r)r$path_id==p$path_id,slopes$johnson_neyman)[[1]]
            inrange<-r$roots[r$roots>=r$observed_range[1] & r$roots<=r$observed_range[2]]
            testthat::expect_equal(inrange,as.numeric(unlist(p$boundaries)),tolerance=1e-8)
            for(w in r$roots) {
                e<-m$edges[[r$path_id]]; f<-eq[[e$target]]$fit; v<-pl_slope_vector(e,eq[[e$target]],list(w=w))
                t<-sum(v*f$coefficients)/sqrt(as.numeric(t(v)%*%f$covariance%*%v))
                prob<-2*if(robust)pnorm(-abs(t)) else pt(-abs(t),f$df_residual)
                testthat::expect_equal(prob,.05,tolerance=1e-8)
            }
        }
        interaction<-pl_interaction_tests(eq,robust)
        for(p in py$interaction_tests) {
            r<-Filter(function(r)r$target_id==p$target_id,interaction)[[1]]
            for(stat in c("statistic","p","deltaR2","df_num"))testthat::expect_equal(r[[stat]],p[[stat]],tolerance=1e-8)
            testthat::expect_identical(r$reference,p$reference)
        }
    })
})

testthat::test_that("budget and invalid self-moderation guards reject before resampling",{
    q<-jsonlite::fromJSON(file.path(fixture,"first_stage-request.json"),simplifyVector=FALSE)
    d<-read.csv(file.path(fixture,"first_stage.csv"))
    m<-validate_spec(q$graph,q$settings,names(d)); s<-prepare_sample(d,m); eq<-fit_equations(compile_equations(m,s))
    testthat::expect_error(bootstrap_estimands(m,s,eq,draws=1e8),"RESOURCE_LIMIT")
    q$graph$moderations[[1]]<-list(id="self_mod",path_id="p_w_m1",moderator_ids=list("w"))
    testthat::expect_error(validate_spec(q$graph,q$settings,names(d)),"INVALID_MODEL")
})
