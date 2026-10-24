pl_nc_dir<-function() {
    root<-getOption("pathlab.root")
    if(!is.null(root))file.path(root,"modules/pathlab/tests/fixtures/categorical-logistic") else testthat::test_path("..","fixtures","categorical-logistic")
}
pl_nc_read<-function(name,suffix)jsonlite::fromJSON(file.path(pl_nc_dir(),paste0(name,"-",suffix,".json")),simplifyVector=FALSE)
pl_nc_class<-function(name)if(exists(name,inherits=TRUE))get(name,inherits=TRUE) else getFromNamespace(name,"pathlab")
pl_nc_fixture<-function(name) {
    q<-pl_nc_read(name,"request")
    d<-read.csv(file.path(pl_nc_dir(),paste0(name,".csv")),check.names=FALSE,stringsAsFactors=FALSE)
    # Model the level metadata delivered by jamovi: retain declared factor
    # order even when it differs from alphabetic order or the reference slot.
    d$w<-factor(d$w,levels=unlist(q$settings$categorical[[2]]$levels))
    byedge<-setNames(q$graph$edges,vapply(q$graph$edges,`[[`,"","id"))
    o<-list(modelMode="general",x="x",y="y",generalMediators=c("m1","m2"),generalModerators=if(q$joint)c("w","z") else "w",generalCovariates=character(),
        generalPaths=lapply(q$graph$edges,function(e)list(source=toupper(e$source),target=toupper(e$target))),
        generalModerations=lapply(q$graph$moderations,function(m) {
            e<-byedge[[m$path_id]];mm<-toupper(unlist(m$moderator_ids))
            list(source=toupper(e$source),target=toupper(e$target),mod1=mm[1],mod2=if(length(mm)>1)mm[2] else "none")
        }),generalProbesW="",generalProbesZ=if(q$joint)"-.5,.7" else "",generalProbesV="",
        categoricalCoding=list(list(role="X",reference="2"),list(role="W",reference="middle")),outcomeFamily=q$settings$outcome_family,logisticEvent="",draws=200L,seed=q$seed,robust=FALSE,confidence=95,showDetails=TRUE,showCovariance=TRUE)
    list(request=q,data=d,options=o)
}
pl_nc_public<-function(f)do.call(pl_nc_class("paths"),c(list(data=f$data),f$options))
pl_nc_result<-function(tables)jsonlite::fromJSON(tables$reproduction$content,simplifyVector=FALSE)
pl_nc_api<-function(f) {
    g<-native_spec(f$options);settings<-native_settings(f$options,g,f$data)
    prepared<-pl_native_prepare_data(f$data,g,f$options)
    pathlab_fit(prepared$data,prepared$spec,settings,draws=f$options$draws,seed=f$options$seed,robust=f$options$robust,alpha=1-f$options$confidence/100)
}
pl_nc_flat<-function(tables) {
    frames<-lapply(tables$effectPages$items,function(t)t$asDF)
    if(!length(frames))return(data.frame())
    out<-do.call(rbind,frames);rownames(out)<-NULL;out
}
pl_nc_matrix<-function(x)do.call(rbind,lapply(x,unlist))
pl_nc_assert_api<-function(tables,r,api) {
    for(target in names(api$equations)) {
        expected<-api$equations[[target]];actual<-r$equations[[target]]
        testthat::expect_identical(actual$family,expected$family)
        testthat::expect_equal(unlist(actual$fit$coefficients),unname(expected$fit$coefficients),ignore_attr=TRUE,tolerance=1e-10)
        testthat::expect_equal(pl_nc_matrix(actual$fit$covariance),unname(expected$fit$covariance),ignore_attr=TRUE,tolerance=1e-10)
    }
    testthat::expect_identical(vapply(r$estimands,`[[`,"","id"),vapply(api$estimands,`[[`,"","id"))
    for(field in c("estimate","bootstrap_se","lower","upper"))testthat::expect_equal(vapply(r$estimands,`[[`,0,field),vapply(api$estimands,`[[`,0,field),tolerance=1e-10)
    testthat::expect_identical(unlist(lapply(tables$effectPages$items,function(page)page$rowKeys),use.names=FALSE),vapply(r$estimands,`[[`,"","id"))
    rows<-pl_nc_flat(tables)
    testthat::expect_equal(nrow(rows),length(r$estimands))
    testthat::expect_equal(rows$estimate,vapply(r$estimands,`[[`,0,"estimate"),tolerance=1e-10)
    testthat::expect_true(all(vapply(tables$effectPages$items,function(page)page$rowCount<=50,TRUE)))
    testthat::expect_equal(tables$effects$rowCount,0)
}
pl_nc_assert_no_cases<-function(r) {
    testthat::expect_null(r$bootstrap_distribution)
    testthat::expect_null(r$sample$values)
    for(eq in r$equations) {
        testthat::expect_null(eq$design);testthat::expect_null(eq$y)
        for(field in c("residuals","fitted","leverage","linear_predictors","weights"))testthat::expect_null(eq$fit[[field]])
        testthat::expect_true(all(lengths(eq$means)==1L))
        testthat::expect_false(any(grepl("@",names(eq$means),fixed=TRUE)))
    }
}
pl_nc_cache<-new.env(parent=emptyenv())
pl_nc_baseline<-function(name) {
    if(!exists(name,envir=pl_nc_cache,inherits=FALSE)) {
        f<-pl_nc_fixture(name);tables<-pl_nc_public(f)
        pl_nc_cache[[name]]<-list(fixture=f,tables=tables,result=pl_nc_result(tables))
    }
    pl_nc_cache[[name]]
}

testthat::test_that("native category arrays retain assigned variables typed level order and explicit references",{
    testthat::skip_if_not_installed("jmvcore")
    f<-pl_nc_fixture("joint_logistic");o<-do.call(pl_nc_class("pathsOptions")$new,f$options)
    testthat::expect_setequal(unique(unlist(o$varsRequired,use.names=FALSE)),names(f$data))
    testthat::expect_identical(o$categoricalCoding,f$options$categoricalCoding)
    testthat::expect_identical(o$outcomeFamily,"logistic")
    testthat::expect_identical(o$logisticEvent,"")
    g<-native_spec(f$options);s<-native_settings(f$options,g,f$data)
    testthat::expect_equal(s$categorical[[1]],list(node_id="x",levels=1:3,reference=2))
    testthat::expect_identical(s$categorical[[2]],list(node_id="w",levels=c("low","middle"),reference="middle"))
    testthat::expect_identical(s$probes[[1]],list(node_id="z",values=c(-.5,.7)))
    prepared<-pl_native_prepare_data(f$data,g,f$options)
    testthat::expect_identical(prepared$binary$event,1)
    testthat::expect_equal(prepared$data$y,f$data$y)
    # Ordinary text columns have a deterministic alphabetic level order.
    f$data$w<-as.character(f$data$w)
    testthat::expect_identical(native_settings(f$options,g,f$data)$categorical[[2]]$levels,c("low","middle"))
    f<-pl_nc_fixture("categorical_linear");f$data$w<-factor(f$data$w,levels=c("high","middle","low"))
    s<-native_settings(f$options,native_spec(f$options),f$data)
    testthat::expect_identical(s$categorical[[2]]$levels,c("high","middle","low"))
    sample<-prepare_sample(f$data,validate_spec(native_spec(f$options),s,names(f$data)))
    testthat::expect_identical(sample$basis$w,c("w@0","w@2"))
    testthat::expect_identical(sample$basis_description[["w@0"]]$level,"high")
    f$data$w<-factor(f$data$w,levels=c("high","middle","low","unused"))
    s<-native_settings(f$options,native_spec(f$options),f$data)
    testthat::expect_error(prepare_sample(f$data,validate_spec(native_spec(f$options),s,names(f$data))),"INVALID_OPTIONS")
    f<-pl_nc_fixture("categorical_linear");f$options$categoricalCoding<-f$options$categoricalCoding[1]
    testthat::expect_error(native_settings(f$options,native_spec(f$options),f$data),"INVALID_OPTIONS")
    f<-pl_nc_fixture("categorical_linear");f$options$categoricalCoding[[2]]$reference<-"unknown"
    testthat::expect_error(native_settings(f$options,native_spec(f$options),f$data),"INVALID_OPTIONS")
    f<-pl_nc_fixture("categorical_linear");f$options$categoricalCoding<-c(f$options$categoricalCoding,list(list(role="X",reference="1")))
    testthat::expect_error(native_spec(f$options),"INVALID_OPTIONS")
})

for(case in c("categorical_linear","joint_logistic"))local({
    name<-case
    testthat::test_that(paste(name,"actual native public tables agree with API fits and independent reference coefficients"),{
        testthat::skip_if_not_installed("jmvcore")
        baseline<-pl_nc_baseline(name);f<-baseline$fixture;tables<-baseline$tables;r<-baseline$result;api<-pl_nc_api(f)
        pl_nc_assert_api(tables,r,api);pl_nc_assert_no_cases(r)
        testthat::expect_equal(r$sample$n,216)
        testthat::expect_equal(r$bootstrap$successful,200)
        testthat::expect_equal(r$bootstrap$failed,0)
        testthat::expect_length(r$estimands,if(f$request$joint)194 else 90)
        testthat::expect_true(tables$coding$visible)
        coding<-tables$coding$asDF
        testthat::expect_equal(nrow(coding),if(f$request$joint)5 else 6)
        testthat::expect_identical(coding$level[coding$role=="X"],c("1","2","3"))
        testthat::expect_identical(coding$reference[coding$role=="X"],rep("2",3))
        testthat::expect_equal(coding$n[coding$role=="X"],rep(72,3))
        testthat::expect_identical(coding$level[coding$role=="W"],vapply(as.list(levels(f$data$w)),pl_value_label,""))
        testthat::expect_true(any(grepl("Reference",coding$basis,fixed=TRUE)))
        effects<-pl_nc_flat(tables)
        testthat::expect_true(all(grepl("x: 1 vs 2|x: 3 vs 2",effects$route)))
        testthat::expect_true(any(grepl("x [1 vs 2]",tables$estimates$asDF$term,fixed=TRUE)))
        testthat::expect_true(any(grepl("w = \"low\"",effects$condition,fixed=TRUE)))
        reference<-pl_nc_read(name,"independent")
        for(target in names(r$equations)) {
            eq<-r$equations[[target]];order<-match(names(eq$terms),unlist(reference$terms[[target]]))
            independent<-reference$equations$false[[target]]
            testthat::expect_false(anyNA(order))
            testthat::expect_equal(unlist(eq$fit$coefficients),unlist(independent$beta)[order],ignore_attr=TRUE,tolerance=1e-8)
            testthat::expect_equal(pl_nc_matrix(eq$fit$covariance),pl_nc_matrix(independent$covariance)[order,order],ignore_attr=TRUE,tolerance=1e-8)
        }
        if(f$request$logistic) {
            testthat::expect_true(tables$logisticFit$visible)
            fit<-tables$logisticFit$asDF
            testthat::expect_equal(nrow(fit),1)
            testthat::expect_identical(fit$event,"1")
            testthat::expect_identical(fit$nonEvent,"0")
            testthat::expect_identical(fit$converged,"Yes")
            testthat::expect_true(fit$iterations>0)
            testthat::expect_equal(fit$ll,r$equations$y$fit$log_likelihood,tolerance=1e-10)
            testthat::expect_equal(fit$aic,r$equations$y$fit$AIC,tolerance=1e-10)
            testthat::expect_true(is.na(tables$equations$asDF$r2[tables$equations$asDF$target=="y"]))
            testthat::expect_identical(r$association_scale,"log_odds_statistical_path_product")
            testthat::expect_identical(r$residual_correlations$status,"not_applicable")
            testthat::expect_match(tables$review$content,"not probability effects",fixed=TRUE)
        } else {
            testthat::expect_false(tables$logisticFit$visible)
            testthat::expect_equal(tables$logisticFit$rowCount,0)
        }
    })
})

testthat::test_that("native binary event choices invert logistic coefficients and retain explicit text labels",{
    testthat::skip_if_not_installed("jmvcore")
    baseline<-pl_nc_baseline("joint_logistic");f<-baseline$fixture;original<-baseline$result
    f$options$logisticEvent<-"0";flipped_tables<-pl_nc_public(f);flipped<-pl_nc_result(flipped_tables)
    testthat::expect_equal(flipped$binary_coding$event,0)
    testthat::expect_equal(unlist(flipped$equations$y$fit$coefficients),-unlist(original$equations$y$fit$coefficients),tolerance=1e-8)
    testthat::expect_equal(unlist(flipped$equations$m1$fit$coefficients),unlist(original$equations$m1$fit$coefficients),tolerance=1e-10)
    testthat::expect_equal(vapply(flipped$estimands,`[[`,0,"estimate"),-vapply(original$estimands,`[[`,0,"estimate"),tolerance=1e-8)
    testthat::expect_equal(vapply(flipped$estimands,`[[`,0,"lower"),-vapply(original$estimands,`[[`,0,"upper"),tolerance=1e-8)
    testthat::expect_equal(vapply(flipped$estimands,`[[`,0,"upper"),-vapply(original$estimands,`[[`,0,"lower"),tolerance=1e-8)
    f<-baseline$fixture;f$data$y<-factor(ifelse(f$data$y==1,"yes","no"),levels=c("yes","no"));f$options$logisticEvent<-"yes"
    labelled_tables<-pl_nc_public(f);labelled<-pl_nc_result(labelled_tables)
    testthat::expect_identical(unlist(labelled$binary_coding$levels),c("yes","no"))
    testthat::expect_identical(labelled$binary_coding$event,"yes")
    testthat::expect_identical(labelled_tables$logisticFit$asDF$event,"yes")
    testthat::expect_identical(labelled_tables$logisticFit$asDF$nonEvent,"no")
    testthat::expect_equal(unlist(labelled$equations$y$fit$coefficients),unlist(original$equations$y$fit$coefficients),tolerance=1e-10)
    testthat::expect_equal(vapply(labelled$estimands,`[[`,0,"lower"),vapply(original$estimands,`[[`,0,"lower"),tolerance=1e-10)
    pl_nc_assert_no_cases(labelled)
    f$options$logisticEvent<-""
    testthat::expect_error(pl_native_prepare_data(f$data,native_spec(f$options),f$options),"INVALID_OPTIONS")
    f$options$logisticEvent<-"absent"
    testthat::expect_error(pl_native_prepare_data(f$data,native_spec(f$options),f$options),"INVALID_OPTIONS")
})

pl_nc_analysis<-function(f) {
    store<-new.env(parent=emptyenv());store$data<-f$data
    a<-pl_nc_class("pathsClass")$new(options=do.call(pl_nc_class("pathsOptions")$new,f$options))
    a$.setReadDatasetHeaderSource(function(vars)store$data[FALSE,unlist(vars),drop=FALSE])
    a$.setReadDatasetSource(function(vars)store$data[,unlist(vars),drop=FALSE])
    list(analysis=a,store=store)
}
pl_nc_change<-function(a,values) {
    for(name in names(values)) {
        option<-a$options$option(name);option$value<-values[[name]]
        values_env<-a$options$values();values_env[[name]]<-option$value
    }
    a$optionsChangedHandler(names(values))
}
pl_nc_assert_cleared<-function(a) {
    for(name in c("equations","estimates","covariance","coding","logisticFit","simpleSlopes","jn","effects"))testthat::expect_equal(a$results[[name]]$rowCount,0)
    testthat::expect_length(a$results$effectPages$items,0)
    testthat::expect_false(a$results$coding$visible)
    testthat::expect_false(a$results$logisticFit$visible)
    testthat::expect_identical(a$results$reproduction$content,"")
    testthat::expect_identical(a$results$diagram$content,"")
}
testthat::test_that("native categorical logistic lifecycle clears new tables on edits failures and family changes",{
    testthat::skip_if_not_installed("jmvcore")
    f<-pl_nc_fixture("joint_logistic");context<-pl_nc_analysis(f);a<-context$analysis;a$run()
    original<-pl_nc_flat(a$results)
    testthat::expect_false(a$errored)
    testthat::expect_equal(a$results$coding$rowCount,5)
    testthat::expect_equal(a$results$logisticFit$rowCount,1)
    bad<-f$options$categoricalCoding;bad[[1]]$reference<-"42"
    pl_nc_change(a,list(categoricalCoding=bad));pl_nc_assert_cleared(a)
    a$run(noThrow=TRUE)
    testthat::expect_true(a$errored);pl_nc_assert_cleared(a)
    testthat::expect_match(a$results$review$content,"reference",fixed=TRUE)
    pl_nc_change(a,list(categoricalCoding=f$options$categoricalCoding));a$run()
    testthat::expect_false(a$errored)
    testthat::expect_identical(pl_nc_flat(a$results),original)
    # Categorical labels require explicit coding even after a successful fit.
    pl_nc_change(a,list(categoricalCoding=list()));a$run(noThrow=TRUE)
    testthat::expect_true(a$errored);pl_nc_assert_cleared(a)
    testthat::expect_match(a$results$review$content,"explicit categorical coding",fixed=TRUE)
    pl_nc_change(a,list(categoricalCoding=f$options$categoricalCoding));a$run()
    testthat::expect_false(a$errored)
    pl_nc_change(a,list(y=NULL));pl_nc_assert_cleared(a);a$run();pl_nc_assert_cleared(a)
    pl_nc_change(a,list(y="y"));a$run()
    testthat::expect_false(a$errored)
    # Explicitly choose a quantitative numerical score for W and change the
    # outcome family; the old categorical and logistic tables must disappear.
    context$store$data$w<-as.numeric(context$store$data$w)
    a$.setReadDatasetHeaderSource(function(vars)context$store$data[FALSE,unlist(vars),drop=FALSE])
    a$.setReadDatasetSource(function(vars)context$store$data[,unlist(vars),drop=FALSE])
    pl_nc_change(a,list(categoricalCoding=list(),outcomeFamily="linear"));pl_nc_assert_cleared(a);a$run()
    testthat::expect_false(a$errored)
    testthat::expect_equal(a$results$coding$rowCount,0)
    testthat::expect_false(a$results$coding$visible)
    testthat::expect_equal(a$results$logisticFit$rowCount,0)
    testthat::expect_false(a$results$logisticFit$visible)
    current<-pl_nc_result(a$results)
    testthat::expect_true(all(vapply(current$equations,function(eq)eq$family=="linear",TRUE)))
    testthat::expect_false(any(grepl("vs 2",a$results$estimates$asDF$term,fixed=TRUE)))
    testthat::expect_true(nrow(pl_nc_flat(a$results))>0)
    testthat::expect_false(identical(pl_nc_flat(a$results),original))
})

testthat::test_that("actual categorical logistic native fits use one fixed common sample for missing values",{
    testthat::skip_if_not_installed("jmvcore")
    f<-pl_nc_fixture("joint_logistic");f$data$m1[1]<-NA_real_;f$data$w[2]<-NA;f$data$y[3]<-NA_real_
    tables<-pl_nc_public(f);r<-pl_nc_result(tables);api<-pl_nc_api(f)
    pl_nc_assert_api(tables,r,api)
    testthat::expect_equal(r$sample$source_n,216)
    testthat::expect_equal(r$sample$n,213)
    testthat::expect_equal(unlist(r$sample$included_rows),4:216)
    testthat::expect_equal(vapply(r$sample$exclusions,`[[`,0,"source_row"),1:3)
    testthat::expect_identical(lapply(r$sample$exclusions,function(row)unlist(row$missing_node_ids)),list("m1","w","y"))
    testthat::expect_true(all(tables$equations$asDF$n==213))
    for(role in c("X","W"))testthat::expect_equal(sum(tables$coding$asDF$n[tables$coding$asDF$role==role]),213)
    pl_nc_assert_no_cases(r)
})
