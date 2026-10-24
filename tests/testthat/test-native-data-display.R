testthat::test_that("native image scale options reuse fits without admitting unknown controls", {
    h<-pl_cache_harness();a<-h$a;a$run();before<-pl_cache_json(a)
    h$counter$forbid<-TRUE
    for(plot in c("effectPlot","interactionPlot","jnPlot"))for(axis in c("widthScale","heightScale")) {
        name<-paste("results",plot,axis,sep="/")
        a$options$.addOption(jmvcore::OptionNumber$new(name=name,value=1))
        pl_cache_change(a,setNames(list(1.25),name))
        testthat::expect_no_error(a$run())
        testthat::expect_identical(pl_cache_json(a)$fit_cache$fit_id,before$fit_cache$fit_id)
        testthat::expect_identical(pl_cache_science(pl_cache_json(a)),pl_cache_science(before))
        image<-a$results[[plot]];size<-image$size
        testthat::expect_gt(size[[if(axis=="widthScale")"width" else "height"]],if(axis=="widthScale")760 else 560)
    }
    testthat::expect_equal(h$counter$calls,1L)
    testthat::expect_false(any(pl_native_is_presentation(c("results/effectPlot/draws","results/other/widthScale","seed"))))
})

testthat::test_that("delivered numeric row identities survive filters and missing values", {
    f<-pl_cache_fixture();f$data<-f$data[c(2,4,seq.int(7,100)),,drop=FALSE]
    f$data$m1[2]<-NA_real_;h<-pl_cache_harness(f);a<-h$a;a$run();r<-pl_cache_json(a)
    testthat::expect_equal(unlist(r$sample$delivered_row_numbers),c(2,4,7:100))
    testthat::expect_equal(unlist(r$sample$included_row_numbers),c(2,7:100))
    testthat::expect_equal(r$sample$source_n,96)
    testthat::expect_equal(r$sample$n,95)
    testthat::expect_equal(r$sample$exclusions[[1]]$source_row,2)
    testthat::expect_equal(unlist(r$sample$exclusions[[1]]$missing_node_ids),"m1")
    old<-r$fit_cache$fit_id;h$data<-f$data;row.names(h$data)<-paste0("anonymous-label-",seq_len(nrow(h$data)));a$run();r<-pl_cache_json(a)
    testthat::expect_null(r$sample$delivered_row_numbers)
    testthat::expect_null(r$sample$included_row_numbers)
    testthat::expect_false(identical(r$fit_cache$fit_id,old))
    testthat::expect_false(grepl("anonymous-label",a$results$reproduction$content,fixed=TRUE))
})

testthat::test_that("all plots render at small and large sizes without changing coordinates", {
    h<-pl_cache_harness();a<-h$a;a$run();r<-pl_cache_json(a);h$counter$forbid<-TRUE
    for(plot in c("effectPlot","interactionPlot","jnPlot"))for(size in list(c(180,130),c(360,260),c(760,560),c(1100,800))) {
        image<-a$results[[plot]];before<-image$state
        file<-tempfile(fileext=".png")
        grDevices::png(file,width=size[1],height=size[2],res=96)
        testthat::expect_no_error(testthat::expect_true(pl_render_plot(image$state)))
        grDevices::dev.off()
        testthat::expect_gt(file.info(file)$size,500)
        testthat::expect_identical(image$state,before)
        unlink(file)
    }
    testthat::expect_identical(pl_cache_json(a),r)
    testthat::expect_equal(h$counter$calls,1L)
})
