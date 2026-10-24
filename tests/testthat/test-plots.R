pl_plot_fixture_dir<-function() {
    root<-getOption("pathlab.root")
    if(!is.null(root))file.path(root,"modules/pathlab/tests/fixtures") else testthat::test_path("..","fixtures")
}
pl_plot_read<-function(path)jsonlite::fromJSON(path,simplifyVector=FALSE)
pl_plot_ck<-function(c)if(!length(c))"" else paste(sort(vapply(names(c),function(n)paste(n,jsonlite::toJSON(c[[n]],auto_unbox=TRUE,digits=12),sep="="),"")),collapse="|")
pl_plot_same_focal<-function(a,b)isTRUE(all.equal(a,b,check.attributes=FALSE))
pl_plot_native_class<-function(name)if(exists(name,inherits=TRUE))get(name,inherits=TRUE) else getFromNamespace(name,"pathlab")
pl_plot_native_change<-function(a,values) {
    for(name in names(values)) {option<-a$options$option(name);option$value<-values[[name]];env<-a$options$values();env[[name]]<-option$value}
    a$optionsChangedHandler(names(values))
}
pl_plot_cases<-pl_plot_read(file.path(pl_plot_fixture_dir(),"plots/manifest.json"))
for(entry in pl_plot_cases)for(robust in c(FALSE,TRUE))local({
    item<-entry;hc3<-robust
    testthat::test_that(paste("native plot coordinates match Python",item$case,if(hc3)"HC3" else "classical"),{
        base<-pl_plot_fixture_dir();prefix<-file.path(base,item$case)
        q<-pl_plot_read(paste0(prefix,"-request.json"));d<-read.csv(paste0(prefix,".csv"),check.names=FALSE)
        indices<-as.matrix(read.csv(paste0(prefix,"-indices.csv"),header=FALSE))
        r<-pathlab_fit(d,q$graph,q$settings,draws=200,seed=q$seed,robust=hc3,indices=indices)
        # Python parity must also hold after the actual native cache's summary
        # reduction and integrity-checked retrieval, with the same shared draws.
        cached<-pl_fit_cache_read(pl_fit_cache_entry("shared-python-fixture",r),"shared-python-fixture")
        testthat::expect_false(is.null(cached));r<-cached$result
        py<-pl_plot_read(file.path(base,"plots",item$reference))[[tolower(as.character(hc3))]]
        model<-validate_spec(q$graph,q$settings,names(d));sample<-prepare_sample(d,model)
        testthat::expect_equal(r$bootstrap$successful,200)
        for(pp in py$plots) {
            seen<-character()
            for(ci in seq_along(sample$conditions)) {
                key<-pl_plot_ck(sample$conditions[[ci]][setdiff(names(sample$conditions[[ci]]),c(pp$source_id,pp$moderator_id))])
                if(key %in% seen)next
                seen<-c(seen,key)
                o<-list(showInteractionPlot=TRUE,plotSource=pp$source_id,plotTarget=pp$target_id,plotModerator=pp$moderator_id,plotCondition=ci)
                state<-pl_plot_states(r,d,o)$interactionPlot
                testthat::expect_false(is.null(state))
                if(is.null(state))next
                # Coordinate parity alone cannot catch graphics-device errors.
                # Render every selected interaction, including categorical
                # sources with point-only legends, on a real PNG device.
                png<-tempfile(fileext=".png")
                grDevices::png(png,width=760,height=560)
                testthat::expect_no_error(pl_render_plot(state))
                grDevices::dev.off()
                testthat::expect_gt(file.info(png)$size,4000)
                unlink(png)
                points<-Filter(function(p)pl_plot_ck(p$other_conditions)==pl_plot_ck(state$condition),pp$points)
                testthat::expect_gt(length(points),0)
                for(p in points) {
                    k<-which(vapply(state$series,function(s)isTRUE(all.equal(s$moderator_value,p$moderator)),TRUE))
                    testthat::expect_length(k,1)
                    if(!length(k))next
                    x<-if(state$categorical)match(p$x,state$x) else which.min(abs(state$x-p$x))
                    testthat::expect_equal(state$x[x],p$x,tolerance=1e-10)
                    s<-state$series[[k]]
                    testthat::expect_equal(c(s$estimate[x],s$lower[x],s$upper[x]),c(p$predicted,p$lower,p$upper),tolerance=1e-8)
                }
            }
        }
        for(jn in py$johnson_neyman) {
            edge<-model$edges[[jn$path_id]]
            ci<-which(vapply(sample$conditions,function(c)pl_plot_ck(c[setdiff(names(c),c(edge$source,jn$moderator_id))])==pl_plot_ck(jn$other_conditions),TRUE))[1]
            contrast<-if(is.null(jn$focal_contrast))1L else which(vapply(sample$basis[[edge$source]],function(f)identical(sample$basis_description[[f]]$level,jn$focal_contrast$level),TRUE))
            o<-list(showJNPlot=TRUE,plotSource=edge$source,plotTarget=edge$target,plotModerator=jn$moderator_id,plotCondition=ci,plotContrast=contrast)
            plot<-pl_plot_states(r,d,o)
            if(jn$status=="not_applicable") {
                testthat::expect_null(plot$jnPlot)
                testthat::expect_match(paste(plot$notes,collapse=" "),"Categorical moderators")
                next
            }
            state<-plot$jnPlot;testthat::expect_false(is.null(state))
            if(is.null(state))next
            testthat::expect_equal(as.numeric(state$boundaries),as.numeric(unlist(jn$boundaries,use.names=FALSE)),tolerance=1e-8)
            testthat::expect_equal(state$observed_range,unlist(jn$observed_range),tolerance=1e-10)
            testthat::expect_equal(length(state$regions),length(jn$regions))
            for(k in seq_along(state$regions))testthat::expect_equal(state$regions[[k]],jn$regions[[k]],tolerance=1e-8)
            for(p in jn$grid) {
                x<-which.min(abs(state$x-p$moderator))
                testthat::expect_equal(state$x[x],p$moderator,tolerance=1e-10)
                for(field in c("estimate","se","lower","upper"))testthat::expect_equal(state[[field]][x],p[[field]],tolerance=1e-8)
            }
            for(boundary in state$boundaries) {
                x<-which.min(abs(state$x-boundary))
                testthat::expect_lt(min(abs(c(state$lower[x],state$upper[x]))),1e-7)
            }
        }
        # Every selected forest row is copied verbatim from its fitted estimand;
        # compare its identity and intervals with fresh shared-index Python too.
        for(kind in c("specific_indirect","total_indirect","direct","total","indices","contrasts")) {
            first<-pl_plot_states(r,d,list(showEffectPlot=TRUE,plotEffectKind=kind))$effectPlot
            if(is.null(first))next
            ids<-character()
            for(page in seq_len(first$pages)) {
                state<-pl_plot_states(r,d,list(showEffectPlot=TRUE,plotEffectKind=kind,plotEffectPage=page))$effectPlot
                for(row in state$rows) {
                    original<-Filter(function(v)v$id==row$id,r$estimands)[[1]]
                    testthat::expect_equal(row[c("estimate","lower","upper")],original[c("estimate","lower","upper")],tolerance=0)
                    if(kind %in% c("specific_indirect","total_indirect","direct","total")) {
                        found<-Filter(function(p)p$kind==original$kind && pl_plot_ck(p$condition)==pl_plot_ck(original$condition) && pl_plot_same_focal(p$focal_contrast,original$focal_contrast) && (p$kind!="specific_indirect" || setequal(unlist(p$path_ids),unlist(original$path_ids))),py$effects)
                        testthat::expect_length(found,1)
                        if(length(found))for(field in c("estimate","lower","upper"))testthat::expect_equal(row[[field]],found[[1]][[field]],tolerance=1e-8)
                    }
                    ids<-c(ids,row$id)
                }
            }
            testthat::expect_equal(length(ids),first$total)
            testthat::expect_false(anyDuplicated(ids)>0)
        }
    })
})

testthat::test_that("logistic interaction links and JN bands match independent glm algebra",{
    base<-file.path(pl_plot_fixture_dir(),"plots/logistic");d<-read.csv(paste0(base,".csv"));q<-pl_plot_read(paste0(base,"-request.json"))
    for(robust in c(FALSE,TRUE)) {
        model<-validate_spec(q$graph,q$settings,names(d));sample<-prepare_sample(d,model);eq<-fit_equations(compile_equations(model,sample),robust)
        fit<-stats::glm(y~w+x+w:x,data=d,family=stats::binomial(),control=stats::glm.control(epsilon=1e-12,maxit=100));X<-stats::model.matrix(fit)
        # Recompute information at the converged probabilities: glm's cached
        # final working QR can still reflect the preceding IRLS iteration.
        weights<-fit$fitted.values*(1-fit$fitted.values);bread<-solve(crossprod(X,X*weights))
        covariance<-bread
        if(robust) {
            leverage<-weights*rowSums((X%*%bread)*X)
            covariance<-bread%*%crossprod(X,X*((d$y-fit$fitted.values)/(1-leverage))^2)%*%bread
        }
        order<-match(names(eq$y$terms),colnames(X));testthat::expect_false(anyNA(order))
        testthat::expect_equal(unname(eq$y$fit$coefficients),unname(stats::coef(fit)[order]),tolerance=1e-7)
        testthat::expect_equal(unname(eq$y$fit$covariance),unname(covariance[order,order]),tolerance=1e-7)
        r<-list(graph=q$graph,requested_settings=q$settings,equations=eq,provenance=list(alpha=.05),johnson_neyman=pl_slopes(model,sample,eq,.05)$johnson_neyman)
        a<-pl_plot_states(r,d,list(showInteractionPlot=TRUE,showJNPlot=TRUE,plotResponseScale="link"))
        b<-pl_plot_states(r,d,list(showInteractionPlot=TRUE,plotResponseScale="response"))
        for(k in seq_along(a$interactionPlot$series))for(field in c("estimate","lower","upper"))testthat::expect_equal(b$interactionPlot$series[[k]][[field]],stats::plogis(a$interactionPlot$series[[k]][[field]]),tolerance=0)
        testthat::expect_identical(a$jnPlot$y_label,"Conditional path slope (log odds)")
        testthat::expect_true(all(unlist(lapply(b$interactionPlot$series,function(s)c(s$lower,s$upper)))>=0 & unlist(lapply(b$interactionPlot$series,function(s)c(s$lower,s$upper)))<=1))
    }
})

testthat::test_that("native Images render and clear their state on option data and failure transitions",{
    testthat::skip_if_not_installed("jmvcore")
    d<-read.csv(file.path(pl_plot_fixture_dir(),"first_stage.csv"))
    options<-pl_plot_native_class("pathsOptions")$new(x="x",m1="m1",m2="m2",y="y",w="w",draws=200,robust=FALSE,showEffectPlot=TRUE,showInteractionPlot=TRUE,showJNPlot=TRUE,plotTarget="M1")
    a<-pl_plot_native_class("pathsClass")$new(options=options)
    a$.setReadDatasetHeaderSource(function(vars)d[FALSE,unlist(vars),drop=FALSE]);a$.setReadDatasetSource(function(vars)d[,unlist(vars),drop=FALSE]);a$run()
    for(name in c("effectPlot","interactionPlot","jnPlot")) {
        image<-a$results[[name]]
        if(name=="jnPlot")testthat::expect_identical(image$state$title,"Johnson\u2013Neyman: x \u2192 m1")
        if(name=="effectPlot")testthat::expect_identical(image$state$title,"Conditional associations \u2014 page 1 of 1")
        testthat::expect_true(image$visible);testthat::expect_false(is.null(image$state))
        file<-tempfile(fileext=".svg")
        grDevices::svg(file,width=760/96,height=560/96)
        testthat::expect_true(pl_render_plot(image$state));grDevices::dev.off()
        testthat::expect_gt(file.info(file)$size,4000)
        unlink(file)
    }
    pl_plot_native_change(a,list(showEffectPlot=FALSE,showInteractionPlot=FALSE,showJNPlot=FALSE))
    for(name in c("effectPlot","interactionPlot","jnPlot")) {testthat::expect_false(a$results[[name]]$visible);testthat::expect_null(a$results[[name]]$state)}
    a$run();testthat::expect_identical(a$results$plotNotes$content,"")
    pl_plot_native_change(a,list(showEffectPlot=TRUE,showInteractionPlot=TRUE,showJNPlot=TRUE));a$run()
    before<-a$results$jnPlot$state
    d$m1[1]<-NA_real_;a$run();testthat::expect_false(isTRUE(all.equal(before,a$results$jnPlot$state)))
    pl_plot_native_change(a,list(p_w_m1=FALSE));a$run(noThrow=TRUE)
    testthat::expect_true(a$errored)
    for(name in c("effectPlot","interactionPlot","jnPlot")) {testthat::expect_false(a$results[[name]]$visible);testthat::expect_null(a$results[[name]]$state)}
})

testthat::test_that("plot selection gives explicit applicability notes and leaves fitted results intact",{
    base<-file.path(pl_plot_fixture_dir(),"first_stage");q<-pl_plot_read(paste0(base,"-request.json"));d<-read.csv(paste0(base,".csv"))
    r<-pathlab_fit(d,q$graph,q$settings,draws=200,robust=FALSE)
    for(o in list(list(showEffectPlot=TRUE,plotEffectPage=8000),list(showJNPlot=TRUE,plotTarget="M10"),list(showJNPlot=TRUE),list(showJNPlot=TRUE,plotTarget="M1",plotContrast=9),list(showInteractionPlot=TRUE,plotTarget="M1",plotCondition=25))) {
        p<-pl_plot_states(r,d,o)
        testthat::expect_null(p$effectPlot);testthat::expect_null(p$interactionPlot);testthat::expect_null(p$jnPlot);testthat::expect_gt(length(p$notes),0)
    }
    testthat::expect_true(is.null(pl_plot_states(r,d,list())$effectPlot))
})
