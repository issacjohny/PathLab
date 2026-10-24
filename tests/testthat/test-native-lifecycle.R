# Exercise real jmvcore R6 results rather than mocking the table API. The input
# fixture is synthetic; no user's Desktop session is loaded by these tests.
pl_native_class <- function(name) {
    if (exists(name, inherits=TRUE)) get(name, inherits=TRUE)
    else getFromNamespace(name,"pathlab")
}
pl_native_fixture <- function() {
    root<-getOption("pathlab.root")
    file<-if(!is.null(root)) file.path(root,"modules/pathlab/tests/fixtures/first_stage.csv") else testthat::test_path("..","fixtures","first_stage.csv")
    read.csv(file,check.names=FALSE)
}
pl_native_analysis <- function(data=pl_native_fixture()) {
    options<-pl_native_class("pathsOptions")$new(x="x",m1="m1",m2="m2",y="y",w="w",draws=200,robust=FALSE)
    a<-pl_native_class("pathsClass")$new(options=options)
    a$.setReadDatasetHeaderSource(function(vars)data[FALSE,unlist(vars),drop=FALSE])
    a$.setReadDatasetSource(function(vars)data[,unlist(vars),drop=FALSE])
    a
}
pl_expect_empty_native_outputs <- function(a) {
    for(name in c("equations","estimates","effects","covariance")) testthat::expect_equal(a$results[[name]]$rowCount,0)
    testthat::expect_identical(a$results$diagnostics$content,"")
    testthat::expect_identical(a$results$reproduction$content,"")
    pl_expect_native_diagram(a,FALSE)
}
pl_expect_native_diagram <- function(a,visible) {
    get("initProtoBuf",asNamespace("jmvcore"))()
    diagram<-a$results$diagram;pb<-diagram$asProtoBuf()
    testthat::expect_identical(diagram$visible,visible)
    # jamovi's Html renderer does not replace its DOM when content is empty.
    # Explicit visibility must therefore travel through the native protocol,
    # removing an old graph on incomplete/error states and restoring a new one.
    testthat::expect_equal(as.integer(pb$visible),if(visible)2L else 3L)
    testthat::expect_identical(pb$html$content,diagram$content)
    if(visible)testthat::expect_match(diagram$content,"<svg",fixed=TRUE) else testthat::expect_identical(diagram$content,"")
}
pl_native_change <- function(a,values) {
    for(name in names(values)) {
        option<-a$options$option(name)
        option$value<-values[[name]]
        env<-a$options$values()
        env[[name]]<-option$value
    }
    a$optionsChangedHandler(names(values))
}

testthat::test_that("native reruns replace every dynamic table row",{
    testthat::skip_if_not_installed("jmvcore")
    a<-pl_native_analysis()
    a$run()
    pl_expect_native_diagram(a,TRUE)
    before<-lapply(c("equations","estimates","effects","covariance"),function(name)a$results[[name]]$asDF)
    testthat::expect_equal(nrow(before[[2]]),11)
    a$run()
    after<-lapply(c("equations","estimates","effects","covariance"),function(name)a$results[[name]]$asDF)
    testthat::expect_equal(after,before,tolerance=0)
    pl_native_change(a,list(p_x_y=FALSE))
    pl_expect_empty_native_outputs(a)
    testthat::expect_match(a$results$review$content,"Updating results")
    a$run()
    testthat::expect_equal(a$results$estimates$rowCount,10)
    testthat::expect_false(any(a$results$estimates$asDF$target=="y" & a$results$estimates$asDF$term=="x"))
    pl_expect_native_diagram(a,TRUE)
})

testthat::test_that("native incomplete and failed specifications clear earlier output",{
    testthat::skip_if_not_installed("jmvcore")
    a<-pl_native_analysis()
    a$run()
    pl_expect_native_diagram(a,TRUE)
    pl_native_change(a,list(y=NULL))
    a$run()
    pl_expect_empty_native_outputs(a)
    testthat::expect_match(a$results$review$content,"Select X and Y",fixed=TRUE)
    testthat::expect_match(a$results$review$content,"No model has been fitted")
    pl_native_change(a,list(y="y"))
    a$run()
    testthat::expect_equal(a$results$estimates$rowCount,11)
    pl_expect_native_diagram(a,TRUE)
    pl_native_change(a,list(p_w_m1=FALSE))
    a$run(noThrow=TRUE)
    testthat::expect_true(a$errored)
    pl_expect_empty_native_outputs(a)
    testthat::expect_match(a$results$review$content,"could not fit")
    pl_native_change(a,list(p_w_m1=TRUE))
    a$run()
    testthat::expect_false(a$errored)
    testthat::expect_equal(a$results$estimates$rowCount,11)
    pl_expect_native_diagram(a,TRUE)
})

testthat::test_that("native data refresh recomputes a common missing-value sample",{
    testthat::skip_if_not_installed("jmvcore")
    d<-pl_native_fixture()
    options<-pl_native_class("pathsOptions")$new(x="x",m1="m1",m2="m2",y="y",w="w",draws=200,robust=FALSE)
    a<-pl_native_class("pathsClass")$new(options=options)
    a$.setReadDatasetHeaderSource(function(vars)d[FALSE,unlist(vars),drop=FALSE])
    a$.setReadDatasetSource(function(vars)d[,unlist(vars),drop=FALSE])
    a$run()
    pl_expect_native_diagram(a,TRUE)
    before<-a$results$estimates$asDF$estimate
    d$m1[c(1,4)]<-NA_real_
    d$y[7]<-NA_real_
    a$run()
    testthat::expect_equal(a$results$estimates$rowCount,11)
    pl_expect_native_diagram(a,TRUE)
    testthat::expect_true(all(a$results$equations$asDF$n==97))
    testthat::expect_gt(max(abs(a$results$estimates$asDF$estimate-before)),1e-5)
    r<-jsonlite::fromJSON(a$results$reproduction$content,simplifyVector=FALSE)
    testthat::expect_equal(r$sample$n,97)
    testthat::expect_equal(unlist(r$sample$included_rows),setdiff(seq_len(100),c(1,4,7)))
    testthat::expect_equal(vapply(r$sample$exclusions,`[[`,0L,"source_row"),c(1,4,7))
})
