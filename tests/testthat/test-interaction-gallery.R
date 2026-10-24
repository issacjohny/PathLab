testthat::test_that("all declared interaction images reuse the exact completed fit",{
    f<-pl_cache_fixture()
    f$options$generalPaths<-c(f$options$generalPaths,list(list(source="W",target="M2"),list(source="W",target="Y")))
    f$options$generalModerations<-c(f$options$generalModerations,lapply(c("M2","Y"),function(target)list(source="X",target=target,mod1="W",mod2="none")))
    h<-pl_cache_harness(f);a<-h$a;a$run();before<-pl_cache_json(a)
    h$counter$forbid<-TRUE
    pl_cache_change(a,list(plotInteractionMode="all"));testthat::expect_no_error(a$run())
    r<-pl_cache_json(a)
    testthat::expect_identical(pl_cache_science(r),pl_cache_science(before))
    testthat::expect_identical(r$fit_cache$fit_id,before$fit_cache$fit_id)
    testthat::expect_equal(h$counter$calls,1L)
    testthat::expect_length(a$results$interactionPlots$items,3)
    testthat::expect_null(r$plots$interactionPlot)
    testthat::expect_equal(r$plots$interactionPagination$total,3)
    for(i in 1:3) {
        image<-a$results$interactionPlots$items[[i]];state<-image$state
        one<-pl_plot_states(a$results$fitCache$state$result,f$data,list(showInteractionPlot=TRUE,plotSource="X",plotTarget=c("M1","M2","Y")[i],plotModerator="W"))$interactionPlot
        state$title<-one$title
        testthat::expect_identical(state,one)
        png<-tempfile(fileext=".png");grDevices::png(png,width=760,height=560)
        testthat::expect_no_error(pl_render_plot(state));grDevices::dev.off()
        testthat::expect_gt(file.info(png)$size,4000);unlink(png)
    }
    pl_cache_change(a,list(plotInteractionPage=2L));a$run()
    testthat::expect_length(a$results$interactionPlots$items,0)
    testthat::expect_match(a$results$plotNotes$content,"choose a page")
    pl_cache_change(a,list(plotInteractionPage=1L));a$run()
    testthat::expect_length(a$results$interactionPlots$items,3)
    file<-tempfile(fileext=".pb");on.exit(unlink(file),add=TRUE)
    a$.setStatePathSource(function()file);pl_cache_wire_options(a$options);a$.save()
    f$options$plotInteractionMode<-"all";fresh<-pl_cache_harness(f,h$counter)$a
    fresh$.setStatePathSource(function()file);fresh$.load()
    testthat::expect_no_error(fresh$run())
    testthat::expect_length(fresh$results$interactionPlots$items,3)
    testthat::expect_identical(pl_cache_json(fresh)$fit_cache$fit_id,r$fit_cache$fit_id)
    pl_cache_change(a,list(showInteractionPlot=FALSE));a$run()
    testthat::expect_length(a$results$interactionPlots$items,0)
    testthat::expect_equal(h$counter$calls,1L)
})

testthat::test_that("gallery resize options preserve cache while unknown paths do not",{
    testthat::expect_true(all(pl_native_is_presentation(c("plotInteractionMode","plotInteractionPage",'results/interactionPlots/"1"/widthScale','results/interactionPlots/"6"/heightScale'))))
    testthat::expect_false(pl_native_is_presentation("results/interactionPlots/[1]/1/unrecognized"))
})

testthat::test_that("actual gallery image addresses retain size and fit through wire save load",{
    f<-pl_cache_fixture();f$options$plotInteractionMode<-"all"
    h<-pl_cache_harness(f);a<-h$a;a$run();before<-pl_cache_json(a)
    image<-a$results$interactionPlots$items[[1]]
    paths<-paste0("results/",image$path,"/",c("widthScale","heightScale"))
    testthat::expect_true(all(pl_native_is_presentation(paths)))
    testthat::expect_false(pl_native_is_presentation(paste0("results/",image$path,"/unknown")))
    scales<-setNames(list(710/760,530/560),paths)
    for(name in names(scales))a$options$.addOption(get("Option",asNamespace("jmvcore"))$new(name,scales[[name]]))
    h$counter$forbid<-TRUE
    pl_cache_change(a,scales);testthat::expect_no_error(a$run())
    testthat::expect_identical(pl_cache_json(a)$fit_cache$fit_id,before$fit_cache$fit_id)
    testthat::expect_identical(pl_cache_science(pl_cache_json(a)),pl_cache_science(before))
    image<-a$results$interactionPlots$items[[1]]
    testthat::expect_equal(unlist(image$size),c(width=710L,height=530L))
    testthat::expect_equal(image$asProtoBuf(status=3L)$image$width,710L)
    testthat::expect_equal(image$asProtoBuf(status=3L)$image$height,530L)
    file<-tempfile(fileext=".pb");on.exit(unlink(file),add=TRUE)
    pl_cache_wire_options(a$options);a$.setStatePathSource(function()file);a$.save()
    fresh<-pl_cache_harness(f,h$counter)$a
    fresh$options$fromProtoBuf(a$options$asProtoBuf())
    fresh$.setStatePathSource(function()file);fresh$.load()
    testthat::expect_no_error(fresh$run())
    image<-fresh$results$interactionPlots$items[[1]]
    testthat::expect_equal(unlist(image$size),c(width=710L,height=530L))
    testthat::expect_identical(pl_cache_json(fresh)$fit_cache$fit_id,before$fit_cache$fit_id)
    testthat::expect_equal(h$counter$calls,1L)
})

testthat::test_that("gallery pages retain every slice and cached fit",{
    f<-pl_cache_fixture();f$data$z<-sin(seq_len(nrow(f$data)))
    arrows<-Filter(function(e)e$source!="W",f$options$generalPaths)
    f$options$generalModerators<-c("w","z")
    f$options$generalProbesW<-"0";f$options$generalProbesZ<-"0"
    main<-unlist(lapply(c("W","Z"),function(w)lapply(c("M1","M2","Y"),function(t)list(source=w,target=t))),recursive=FALSE)
    f$options$generalPaths<-c(arrows,main)
    f$options$generalModerations<-unlist(lapply(arrows,function(e)lapply(c("W","Z"),function(w)list(source=e$source,target=e$target,mod1=w,mod2="none"))),recursive=FALSE)
    f$options$plotInteractionMode<-"all"
    h<-pl_cache_harness(f);a<-h$a;a$run();r<-pl_cache_json(a)
    testthat::expect_equal(r$plots$interactionPagination$total,12)
    testthat::expect_equal(r$plots$interactionPagination$pages,2)
    testthat::expect_length(a$results$interactionPlots$items,6)
    keys<-names(r$plots$interactionPlots);h$counter$forbid<-TRUE
    pl_cache_change(a,list(plotInteractionPage=2L));a$run();after<-pl_cache_json(a)
    testthat::expect_length(a$results$interactionPlots$items,6)
    testthat::expect_equal(length(union(keys,names(after$plots$interactionPlots))),12)
    testthat::expect_identical(pl_cache_science(r),pl_cache_science(after))
    testthat::expect_identical(r$fit_cache$fit_id,after$fit_cache$fit_id)
    testthat::expect_equal(h$counter$calls,1L)
})
