pl_dd_dir<-function() {
    root<-getOption("pathlab.root")
    if(is.null(root))testthat::test_path("..","fixtures") else file.path(root,"modules/pathlab/tests/fixtures")
}
pl_dd_manifest<-jsonlite::fromJSON(file.path(pl_dd_dir(),"data-display/manifest.json"),simplifyVector=FALSE)
for(item in pl_dd_manifest$scenarios)local({
    entry<-item
    testthat::test_that(paste("delivered",entry$name,"matches shared-index Python and independent equations"), {
        dir<-pl_dd_dir();q<-jsonlite::fromJSON(file.path(dir,"first_stage-request.json"),simplifyVector=FALSE)
        prefix<-file.path(dir,"data-display",entry$name)
        d<-read.csv(paste0(prefix,".csv"));indices<-as.matrix(read.csv(paste0(prefix,"-indices.csv"),header=FALSE))
        py<-jsonlite::fromJSON(paste0(prefix,"-python.json"),simplifyVector=FALSE)
        r<-pathlab_fit(d,q$graph,q$settings,draws=200L,robust=FALSE,indices=indices)
        testthat::expect_equal(r$sample$source_n,entry$delivered_rows)
        testthat::expect_equal(r$sample$n,entry$n)
        testthat::expect_equal(r$bootstrap$successful,200L)
        complete<-d[complete.cases(d),,drop=FALSE]
        for(pe in py$equations) {
            e<-r$equations[[pe$target_id]]
            independent<-stats::lm(stats::as.formula(paste(pe$target_id,"~",paste(unlist(q$reference_equations[[pe$target_id]]),collapse="+"))),complete)
            testthat::expect_equal(unname(e$fit$coefficients),unname(stats::coef(independent)),tolerance=1e-10)
            testthat::expect_equal(unname(e$fit$covariance),unname(stats::vcov(independent)),tolerance=1e-10)
            testthat::expect_equal(unname(e$fit$coefficients),vapply(pe$coefficients,`[[`,0,"estimate"),tolerance=1e-8)
            testthat::expect_equal(unname(e$fit$covariance),unname(do.call(rbind,lapply(pe$covariance$values,unlist))),tolerance=1e-8)
        }
        ck<-function(x)if(!length(x))"" else paste(sort(paste(names(x),sprintf("%.10g",unlist(x)),sep="=")),collapse="|")
        for(p in py$effects) {
            rows<-Filter(function(a)a$kind==p$kind && ck(a$condition)==ck(p$condition) && (a$kind!="specific_indirect" || setequal(a$path_ids,unlist(p$path_ids))),r$estimands)
            testthat::expect_length(rows,1L)
            for(stat in c("estimate","bootstrap_se","lower","upper"))testthat::expect_equal(rows[[1]][[stat]],p[[stat]],tolerance=1e-8)
        }
    })
})
