pl_cancel_fixture<-function(name="first_stage") {
    root<-getOption("pathlab.root")
    folder<-if(!is.null(root))file.path(root,"modules/pathlab/tests/fixtures") else testthat::test_path("..","fixtures")
    q<-jsonlite::fromJSON(file.path(folder,paste0(name,"-request.json")),simplifyVector=FALSE)
    list(q=q,data=read.csv(file.path(folder,paste0(name,".csv"))),indices=as.matrix(read.csv(file.path(folder,paste0(name,"-indices.csv")),header=FALSE)))
}

testthat::test_that("progress preserves exact saved-index linear and logistic bootstrap results", {
    for(name in c("first_stage","plots/logistic")) {
        f<-pl_cancel_fixture(name);q<-f$q
        base<-pathlab_fit(f$data,q$graph,q$settings,draws=200L,robust=FALSE,indices=f$indices)
        seen<-integer()
        observed<-pathlab_fit(f$data,q$graph,q$settings,draws=200L,robust=FALSE,indices=f$indices,
            progress=function(done,total) {testthat::expect_equal(total,200L);seen<<-c(seen,done)})
        testthat::expect_identical(observed,base)
        testthat::expect_identical(seen,c(0L,seq.int(9L,199L,10L),200L))
    }
})

testthat::test_that("cancellation escapes the draw failure handler and restores RNG", {
    f<-pl_cancel_fixture();q<-f$q
    set.seed(917L);before<-get(".Random.seed",.GlobalEnv);kind<-RNGkind();seen<-integer()
    cancel<-structure(list(message="Test cancellation",call=NULL,code="restart"),class=c("pathlab_test_cancel","error","condition"))
    caught<-tryCatch(pathlab_fit(f$data,q$graph,q$settings,draws=200L,robust=FALSE,
        progress=function(done,total) {seen<<-c(seen,done);if(done>=19L)stop(cancel)}),pathlab_test_cancel=function(e)e)
    testthat::expect_identical(caught,cancel)
    testthat::expect_identical(seen,c(0L,9L,19L))
    testthat::expect_identical(get(".Random.seed",.GlobalEnv),before)
    testthat::expect_identical(RNGkind(),kind)
    testthat::expect_error(pathlab_fit(f$data,q$graph,q$settings,draws=200L,progress=TRUE),"progress must")
})

testthat::test_that("native restart publishes no partial fit and recovers cleanly", {
    h<-pl_cache_harness();a<-h$a;calls<-0L;flushed<-0L
    a$.setCheckpoint(function(raw) {
        calls<<-calls+1L
        if(!is.null(raw))flushed<<-flushed+1L
        if(calls>=4L)"restart" else NULL
    })
    # Startup check, bootstrap start, draw 9 and draw 19: stop before completion.
    testthat::expect_false(a$run(noThrow=TRUE))
    testthat::expect_identical(calls,4L)
    testthat::expect_gte(flushed,1L)
    testthat::expect_null(a$results$fitCache$state)
    testthat::expect_identical(a$results$reproduction$content,"")
    testthat::expect_equal(nrow(a$results$equations$asDF),0L)
    a$.setCheckpoint(NULL)
    testthat::expect_true(a$run())
    testthat::expect_equal(a$results$fitCache$state$result$bootstrap$successful,200L)
    testthat::expect_identical(h$counter$calls,2L)
})

testthat::test_that("cancelling cached output preserves the completed fit for the next presentation", {
    h<-pl_cache_harness();a<-h$a;a$run();entry<-a$results$fitCache$state
    h$counter$forbid<-TRUE
    pl_cache_change(a,list(showEffectPlot=FALSE))
    a$.setCheckpoint(function(raw)"restart")
    testthat::expect_false(a$run(noThrow=TRUE))
    testthat::expect_identical(a$results$fitCache$state,entry)
    testthat::expect_identical(a$results$reproduction$content,"")
    a$.setCheckpoint(NULL)
    testthat::expect_true(a$run())
    testthat::expect_identical(a$results$fitCache$state,entry)
    r<-jsonlite::fromJSON(a$results$reproduction$content,simplifyVector=FALSE)
    testthat::expect_identical(r$fit_cache$status,"reused")
    testthat::expect_identical(h$counter$calls,1L)
})
