testthat::test_that("all native presentation changes reuse one fit and bootstrap",{
    h<-pl_cache_harness();a<-h$a;a$run();before<-pl_cache_json(a);id<-before$fit_cache$fit_id
    testthat::expect_identical(before$fit_cache$status,"computed");testthat::expect_equal(h$counter$calls,1L)
    h$counter$forbid<-TRUE
    changes<-list(showDetails=TRUE,showCovariance=TRUE,showEffectPlot=FALSE,showInteractionPlot=FALSE,showJNPlot=FALSE,
        plotEffectKind="direct",plotEffectPage=2L,plotSource="M1",plotTarget="M2",plotModerator="Z",plotCondition=2L,plotContrast=2L,plotResponseScale="link",plotInteractionMode="all",plotInteractionPage=2L)
    testthat::expect_setequal(names(changes),pl_native_presentation_fields())
    for(name in names(changes)) {
        pl_cache_change(a,changes[name]);testthat::expect_no_error(a$run())
        r<-pl_cache_json(a)
        testthat::expect_identical(r$fit_cache$status,"reused",info=name)
        testthat::expect_identical(r$fit_cache$fit_id,id,info=name)
        testthat::expect_identical(pl_cache_science(r),pl_cache_science(before),info=name)
    }
    pl_cache_change(a,list(showEffectPlot=TRUE,showInteractionPlot=TRUE,showJNPlot=TRUE,plotEffectKind="specific_indirect",plotEffectPage=1L,plotSource="X",plotTarget="M1",plotModerator="W",plotCondition=1L,plotContrast=1L,plotResponseScale="response",plotInteractionMode="selected",plotInteractionPage=1L));a$run()
    testthat::expect_identical(pl_cache_json(a)$plots,before$plots)
    testthat::expect_equal(h$counter$calls,1L)
})

testthat::test_that("native fit state survives genuine save load and rejects damaged entries",{
    h<-pl_cache_harness();a<-h$a;a$run();id<-pl_cache_json(a)$fit_cache$fit_id
    file<-tempfile(fileext=".pb");on.exit(unlink(file),add=TRUE)
    a$.setStatePathSource(function()file);testthat::expect_no_error(a$.save());testthat::expect_true(file.exists(file))
    f<-pl_cache_fixture();f$options$plotEffectKind<-"direct"
    fresh<-pl_cache_harness(f,h$counter);b<-fresh$a;b$.setStatePathSource(function()file)
    h$counter$forbid<-TRUE
    testthat::expect_no_error(b$.load());testthat::expect_no_error(b$run())
    testthat::expect_identical(pl_cache_json(b)$fit_cache$fit_id,id)
    testthat::expect_identical(pl_cache_json(b)$fit_cache$status,"reused")
    testthat::expect_equal(h$counter$calls,1L)
    h$counter$forbid<-FALSE
    entry<-b$results$fitCache$state;entry$result$estimands[[1]]$estimate<-entry$result$estimands[[1]]$estimate+1
    b$results$fitCache$setState(entry);b$run()
    testthat::expect_equal(h$counter$calls,2L)
    testthat::expect_identical(pl_cache_json(b)$fit_cache$status,"computed")
    testthat::expect_false(identical(pl_cache_json(b)$fit_cache$fit_id,id))
    entry<-b$results$fitCache$state;entry$schema<-0L;b$results$fitCache$setState(entry);b$run()
    testthat::expect_equal(h$counter$calls,3L)
    # A different build/input fingerprint must never reuse the saved fit.
    entry<-b$results$fitCache$state;entry$key<-"previous-build";b$results$fitCache$setState(entry);b$run()
    testthat::expect_equal(h$counter$calls,4L)
})

testthat::test_that("missing-value fits reuse saved state across confidence storage types",{
    f<-pl_cache_fixture();f$data$m1[1:2]<-NA_real_
    h<-pl_cache_harness(f);a<-h$a
    # Exercise the native Number option exactly as delivered by Desktop.
    option<-a$options$option("confidence");option$value<-95.0
    values<-a$options$values();values$confidence<-95.0
    testthat::expect_type(a$options$confidence,"double")
    a$run();before<-pl_cache_json(a)
    testthat::expect_equal(before$sample$n,98)
    file<-tempfile(fileext=".pb");on.exit(unlink(file),add=TRUE)
    a$.setStatePathSource(function()file);a$.save()
    fresh<-pl_cache_harness(f,h$counter);b<-fresh$a
    b$.setStatePathSource(function()file);b$.load()
    option<-b$options$option("confidence");option$value<-95L
    values<-b$options$values();values$confidence<-95L
    testthat::expect_type(b$options$confidence,"integer")
    h$counter$forbid<-TRUE
    testthat::expect_no_error(b$run())
    after<-pl_cache_json(b)
    testthat::expect_identical(after$fit_cache$status,"reused")
    testthat::expect_identical(after$fit_cache$fit_id,before$fit_cache$fit_id)
    testthat::expect_identical(after$fit_cache$input_sha256,before$fit_cache$input_sha256)
    testthat::expect_identical(pl_cache_science(after),pl_cache_science(before))
    testthat::expect_identical(after$plots,before$plots)
    testthat::expect_equal(h$counter$calls,1L)
    h$counter$forbid<-FALSE
    pl_cache_change(b,list(confidence=95.5));b$run()
    testthat::expect_identical(pl_cache_json(b)$fit_cache$status,"computed")
    testthat::expect_equal(h$counter$calls,2L)
    testthat::expect_false(identical(pl_cache_json(b)$fit_cache$fit_id,before$fit_cache$fit_id))
})

testthat::test_that("estimation model and delivered-data edits recompute and failures clear cache",{
    h<-pl_cache_harness();a<-h$a;a$run();count<-1L
    previous<-pl_cache_json(a)$fit_cache$fit_id
    assert_new<-function() {
        a$run();count<<-count+1L;r<-pl_cache_json(a)
        testthat::expect_equal(h$counter$calls,count)
        testthat::expect_identical(r$fit_cache$status,"computed")
        testthat::expect_false(identical(r$fit_cache$fit_id,previous));previous<<-r$fit_cache$fit_id
    }
    for(change in list(list(seed=420L),list(draws=201L),list(robust=TRUE),list(confidence=90),list(generalProbesW="-.5,.5"))) {pl_cache_change(a,change);assert_new()}
    paths<-a$options$generalPaths;paths<-Filter(function(e)!(e$source=="X"&&e$target=="M2"),paths)
    pl_cache_change(a,list(generalPaths=paths));assert_new()
    # Same N and even identical empirical moments cannot justify a cache hit:
    # row order affects the pairing with seeded bootstrap indices.
    h$data$m1[1]<-h$data$m1[1]+.25;assert_new()
    h$data<-h$data[rev(seq_len(nrow(h$data))),,drop=FALSE];assert_new()
    h$data$m1[1]<-NA_real_;assert_new();testthat::expect_equal(pl_cache_json(a)$sample$n,99)
    h$data<-h$data[seq_len(90),,drop=FALSE];assert_new();testthat::expect_equal(pl_cache_json(a)$sample$n,89)
    attr(h$data,"jmv-weights")<-rep(1L,nrow(h$data));a$run(noThrow=TRUE)
    testthat::expect_true(a$errored);testthat::expect_null(a$results$fitCache$state)
    testthat::expect_equal(h$counter$calls,count)
    for(name in c("effectPlot","interactionPlot","jnPlot"))testthat::expect_null(a$results[[name]]$state)
    attr(h$data,"jmv-weights")<-NULL;assert_new()
    pl_cache_change(a,list(categoricalCoding=list(list(role="X",reference="999"))));a$run(noThrow=TRUE)
    testthat::expect_true(a$errored);testthat::expect_null(a$results$fitCache$state)
    testthat::expect_identical(a$results$reproduction$content,"")
    pl_cache_change(a,list(categoricalCoding=list()));assert_new()
    pl_cache_change(a,list(x=NULL));a$run()
    testthat::expect_null(a$results$fitCache$state);testthat::expect_identical(a$results$reproduction$content,"")
})

testthat::test_that("categorical joint and logistic plot changes use cached exact fitted summaries",{
    for(mode in c("categorical-logistic/joint_logistic","plots/logistic")) {
        h<-pl_cache_harness(pl_cache_fixture(mode));a<-h$a;a$run();before<-pl_cache_json(a)
        h$counter$forbid<-TRUE
        changes<-if(mode=="plots/logistic")list(list(plotResponseScale="link"),list(plotResponseScale="response")) else list(list(plotEffectPage=2L),list(plotContrast=2L),list(plotCondition=3L),list(plotModerator="W"))
        for(change in changes) {
            pl_cache_change(a,change);testthat::expect_no_error(a$run());r<-pl_cache_json(a)
            testthat::expect_identical(r$fit_cache$status,"reused")
            testthat::expect_identical(r$fit_cache$fit_id,before$fit_cache$fit_id)
            testthat::expect_identical(pl_cache_science(r),pl_cache_science(before))
        }
        testthat::expect_equal(h$counter$calls,1L)
        if(mode=="plots/logistic")testthat::expect_identical(r$plots,before$plots) else {
            testthat::expect_null(r$plots$jnPlot)
            testthat::expect_true(any(grepl("Categorical moderators",unlist(r$plots$notes),fixed=TRUE)))
            h$counter$forbid<-FALSE;coding<-a$options$categoricalCoding;coding[[1]]$reference<-"1"
            pl_cache_change(a,list(categoricalCoding=coding));a$run()
            testthat::expect_equal(h$counter$calls,2L);testthat::expect_identical(pl_cache_json(a)$fit_cache$status,"computed")
        }
    }
})

testthat::test_that("native cache invalidation schema covers every statistical option and stores no case values",{
    h<-pl_cache_harness();a<-h$a;a$run();entry<-a$results$fitCache$state
    testthat::expect_setequal(a$options$names,c(pl_native_model_fields(),"draws","seed","robust","confidence",pl_native_presentation_fields()))
    pb<-a$results$fitCache$asProtoBuf()
    for(name in c("data",pl_native_model_fields(),"draws","seed","robust","confidence")) {
        fresh<-pl_cache_harness()$a
        fresh$results$fitCache$fromProtoBuf(pb,name,character())
        testthat::expect_null(fresh$results$fitCache$state,info=name)
    }
    fresh<-pl_cache_harness()$a;fresh$results$fitCache$fromProtoBuf(pb,character(),"m1")
    testthat::expect_null(fresh$results$fitCache$state)
    testthat::expect_null(entry$result$bootstrap_distribution);testthat::expect_null(entry$result$sample$values)
    for(eq in entry$result$equations) {
        testthat::expect_null(eq$design);testthat::expect_null(eq$y)
        for(field in c("residuals","fitted","leverage","linear_predictors","weights"))testthat::expect_null(eq$fit[[field]])
    }
    testthat::expect_lt(entry$compressed_bytes,500000)
})
