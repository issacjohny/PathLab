testthat::test_that("data-only cache retains exact numeric types attributes and Unicode",{
    h<-pl_cache_harness();h$a$run();entry<-h$a$results$fitCache$state
    entry$result$codec_test<-list(numbers=c(-0,pi,NA_real_,NaN,Inf,-Inf,.Machine$double.xmin),
        integers=c(1L,NA_integer_),logical=c(TRUE,FALSE,NA),empty=character(),
        matrix=matrix(c(pi,NA_real_,1,2),2,dimnames=list(c("研究","B"),c("CI","β"))),
        labels=c("研究","β",NA_character_),null=NULL)
    entry$payload_sha256<-digest::digest(entry$result,algo="sha256",serializeVersion=2L)
    text<-pl_fit_cache_json(entry)
    testthat::expect_true(nzchar(text))
    testthat::expect_identical(pl_fit_cache_from_json(text,entry$key),entry)
    testthat::expect_null(pl_fit_cache_from_json(text,"changed input"))
    damaged<-entry;damaged$result$estimands[[1]]$estimate<-100
    testthat::expect_null(pl_fit_cache_from_json(pl_fit_cache_json(damaged),entry$key))
    testthat::expect_identical(pl_fit_cache_json(list(environment=new.env())),"")
    wire<-jsonlite::fromJSON(text,simplifyVector=FALSE)
    wire$payload$type<-"language"
    testthat::expect_null(pl_fit_cache_from_json(jsonlite::toJSON(wire,auto_unbox=TRUE),entry$key))
    testthat::expect_null(pl_fit_cache_from_json("{",entry$key))
})

testthat::test_that("fresh engine reuses data-only saved fit after every R state field is dropped",{
    f<-pl_cache_fixture();f$data$m1[1:2]<-NA_real_;f$options$plotInteractionMode<-"all"
    h<-pl_cache_harness(f);a<-h$a;a$run();before<-pl_cache_json(a)
    file<-tempfile(fileext=".pb");on.exit(unlink(file),add=TRUE)
    a$.setStatePathSource(function()file);pl_cache_wire_options(a$options);a$.save()
    core<-asNamespace("jmvcore")
    con<-file(file,"rb");pb<-get("RProtoBuf_read",core)(get("jamovi.coms.AnalysisResponse",core),con);close(con)
    strip<-function(e) {
        e$state<-raw()
        if(e$has("group"))for(child in e$group$elements)strip(child)
        if(e$has("array"))for(child in e$array$elements)strip(child)
    }
    strip(pb$results)
    con<-file(file,"wb");get("RProtoBuf_serialize",core)(pb,con);close(con)
    testthat::expect_identical(pl_fit_cache_saved(file,before$fit_cache$input_sha256)$fit_id,before$fit_cache$fit_id)
    fresh<-pl_cache_harness(f,h$counter)$a
    # Desktop 28 starts a new analysis without calling R .load at this point.
    fresh$.setStatePathSource(function()file);h$counter$forbid<-TRUE
    testthat::expect_null(fresh$results$fitCache$state)
    # The real host saves the initialized, empty result tree before run().
    # Reading the old file only inside .fit is already too late.
    fresh$init();fresh$postInit();pl_cache_wire_options(fresh$options);fresh$.save()
    testthat::expect_identical(pl_fit_cache_saved(file,before$fit_cache$input_sha256)$fit_id,before$fit_cache$fit_id)
    # Initialization and fitting may happen in separate R analysis instances.
    # A private field in the first instance cannot bridge that handoff.
    fresh<-pl_cache_harness(f,h$counter)$a;fresh$.setStatePathSource(function()file)
    testthat::expect_no_error(fresh$run())
    after<-pl_cache_json(fresh)
    testthat::expect_identical(after$fit_cache$status,"reused")
    testthat::expect_identical(after$fit_cache$fit_id,before$fit_cache$fit_id)
    testthat::expect_identical(after$fit_cache$computed_utc,before$fit_cache$computed_utc)
    testthat::expect_identical(pl_cache_science(after),pl_cache_science(before))
    testthat::expect_identical(after$plots,before$plots)
    testthat::expect_equal(h$counter$calls,1L)
    # Explicit statistical changes and altered delivered values still refit.
    h$counter$forbid<-FALSE
    pl_cache_change(fresh,list(seed=420L));fresh$run()
    testthat::expect_equal(h$counter$calls,2L)
    changed<-f;changed$data$m1[3]<-changed$data$m1[3]+.25
    other<-pl_cache_harness(changed,h$counter)$a;other$.setStatePathSource(function()file);other$run()
    testthat::expect_equal(h$counter$calls,3L)
    testthat::expect_false(identical(pl_cache_json(other)$fit_cache$fit_id,before$fit_cache$fit_id))
})
