pl_bulk_class<-function(name)if(exists(name,inherits=TRUE))get(name,inherits=TRUE) else getFromNamespace(name,"pathlab")
pl_bulk_results<-function(mode="general") {
    get("initProtoBuf",asNamespace("jmvcore"))()
    options<-pl_bulk_class("pathsOptions")$new(modelMode=mode)
    results<-pl_bulk_class("pathsResults")$new(options=options)
    results$.update()
    results
}
pl_bulk_spec<-function()list(nodes=list(list(id="x",column="Exposure",role="independent"),list(id="m1",column="M\u00e9diator",role="mediator"),list(id="y",column="Outcome",role="dependent"),list(id="w",column="Moderator",role="moderator")),edges=list(list(id="a",source="x",target="m1"),list(id="b",source="m1",target="y")),moderations=list())
pl_bulk_rows<-function(n)lapply(seq_len(n),function(i)list(id=paste0("effect_",i),kind="specific_indirect",path_ids=c("a","b"),condition=list(w=i/10),moderator_ids=character(),effect_kind="specific_indirect",estimate=if(i==1)NA_real_ else if(i==2)NaN else i/7,bootstrap_se=.1,lower=-i,upper=i))
pl_bulk_flatten<-function(array) {
    result<-do.call(rbind,lapply(array$items,function(t)t$asDF))
    if(!is.null(result))rownames(result)<-NULL
    result
}

testthat::test_that("all-visible native effect blocks preserve every cell and protobuf type",{
    testthat::skip_if_not_installed("jmvcore")
    # RProtoBuf returns UTF-8 text without an encoding mark. Match the Desktop
    # UTF-8 locale instead of interpreting graph arrows as native code-page text.
    withr::local_locale(c(LC_CTYPE=if(.Platform$OS.type=="windows")".UTF-8" else "C.UTF-8"))
    spec<-pl_bulk_spec();rows<-pl_bulk_rows(105)
    rows[[105]]<-list(id="contrast_1_2",kind="route_difference",path_ids=c("a","b"),condition=list(w=.1),condition_b=list(w=.2),effect_a=rows[[1]]$id,effect_b=rows[[2]]$id,estimate=.5,bootstrap_se=.2,lower=-1,upper=2)
    result<-pl_bulk_results();lookup<-setNames(rows,vapply(rows,`[[`,"","id"))
    for(row in rows)result$effects$addRow(row$id,pl_effect_values(spec,row,lookup))
    pl_populate_effect_pages(result$effectPages,spec,rows)
    testthat::expect_length(result$effectPages$items,3)
    testthat::expect_identical(vapply(result$effectPages$items,function(t)t$rowCount,0.0),c(50,50,5))
    expected<-result$effects$asDF;rownames(expected)<-NULL
    testthat::expect_identical(pl_bulk_flatten(result$effectPages),expected)
    testthat::expect_identical(unlist(lapply(result$effectPages$items,function(t)t$rowKeys),use.names=FALSE),vapply(rows,`[[`,"","id"))
    testthat::expect_match(result$effectPages$get(index=3)$title,"rows 101 to 105 of 105",fixed=TRUE)
    testthat::expect_false(result$effects$visible)
    testthat::expect_true(result$effectPages$visible)
    # Desktop supplies the current analysis status when serializing results.
    pb<-result$effectPages$asProtoBuf(status=3L)
    testthat::expect_length(pb$array$elements,3)
    testthat::expect_equal(pb$array$layout,0)
    testthat::expect_equal(as.integer(pb$status),3L)
    testthat::expect_true(all(vapply(pb$array$elements,function(p)as.integer(p$status),0L)==3L))
    testthat::expect_equal(as.integer(pb$visible),0L)
    testthat::expect_equal(as.integer(result$effects$asProtoBuf()$visible),1L)
    restored<-pl_bulk_results()
    for(page in seq_along(result$effectPages$items)) {
        source<-result$effectPages$get(index=page)
        target<-restored$effectPages$addItem(page)
        target$setTitle(source$title)
        for(key in source$rowKeys)target$addRow(key)
    }
    restored$effectPages$fromProtoBuf(pb,character(),character())
    testthat::expect_identical(pl_bulk_flatten(restored$effectPages),expected)
    for(page in seq_along(result$effectPages$items)) {
        source<-result$effectPages$get(index=page);target<-restored$effectPages$get(index=page)
        for(i in seq_len(source$rowCount))for(col in names(expected))testthat::expect_identical(target$getCell(col,rowNo=i)$value,source$getCell(col,rowNo=i)$value)
    }
    legacy<-pl_bulk_results("legacy")
    testthat::expect_true(legacy$effects$visible)
    testthat::expect_false(legacy$effectPages$visible)
    pl_populate_effect_pages(result$effectPages,spec,list())
    testthat::expect_length(result$effectPages$items,0)
})

testthat::test_that("thousand-row native output is bounded to fifty rows per visible table",{
    testthat::skip_if_not_installed("jmvcore")
    result<-pl_bulk_results();rows<-pl_bulk_rows(1001)
    elapsed<-system.time(pl_populate_effect_pages(result$effectPages,pl_bulk_spec(),rows))[3]
    testthat::expect_length(result$effectPages$items,21)
    testthat::expect_true(all(vapply(result$effectPages$items,function(t)t$rowCount,0.0)<=50L))
    flattened<-pl_bulk_flatten(result$effectPages)
    testthat::expect_equal(nrow(flattened),1001)
    testthat::expect_identical(flattened$estimate,vapply(rows,`[[`,0.0,"estimate"))
    testthat::expect_identical(unlist(lapply(result$effectPages$items,function(t)t$rowKeys),use.names=FALSE),vapply(rows,`[[`,"","id"))
    testthat::expect_length(result$effectPages$asProtoBuf()$array$elements,21)
    cat("\nNative 1001-row all-visible output elapsed seconds:",unname(elapsed),"\n")
})

pl_bulk_general_analysis<-function() {
    root<-getOption("pathlab.root")
    folder<-if(!is.null(root))file.path(root,"modules/pathlab/tests/fixtures/native-general") else testthat::test_path("..","fixtures","native-general")
    q<-jsonlite::fromJSON(file.path(folder,"joint3_two-request.json"),simplifyVector=FALSE)
    d<-read.csv(file.path(folder,"joint3_two.csv"),check.names=FALSE)
    o<-q$native_options
    for(name in c("generalMediators","generalModerators","generalCovariates"))o[[name]]<-unlist(o[[name]],use.names=FALSE)
    analysis<-pl_bulk_class("pathsClass")$new(options=do.call(pl_bulk_class("pathsOptions")$new,o))
    analysis$.setReadDatasetHeaderSource(function(vars)d[FALSE,unlist(vars),drop=FALSE])
    analysis$.setReadDatasetSource(function(vars)d[,unlist(vars),drop=FALSE])
    analysis
}
pl_bulk_change<-function(a,values) {
    for(name in names(values)) {
        option<-a$options$option(name);option$value<-values[[name]]
        values_env<-a$options$values();values_env[[name]]<-option$value
    }
    a$optionsChangedHandler(names(values))
}
testthat::test_that("general native reruns and incomplete or failed models clear all effect blocks",{
    testthat::skip_if_not_installed("jmvcore")
    a<-pl_bulk_general_analysis();a$run()
    before<-pl_bulk_flatten(a$results$effectPages)
    testthat::expect_length(a$results$effectPages$items,9)
    testthat::expect_equal(nrow(before),409)
    testthat::expect_equal(a$results$effects$rowCount,0)
    a$run()
    testthat::expect_identical(pl_bulk_flatten(a$results$effectPages),before)
    pl_bulk_change(a,list(y=NULL))
    testthat::expect_length(a$results$effectPages$items,0)
    a$run()
    testthat::expect_length(a$results$effectPages$items,0)
    pl_bulk_change(a,list(y="y"));a$run()
    testthat::expect_length(a$results$effectPages$items,9)
    original<-a$options$generalPaths
    invalid<-Filter(function(row)!(row$source=="W" && row$target=="M1"),original)
    pl_bulk_change(a,list(generalPaths=invalid));a$run(noThrow=TRUE)
    testthat::expect_true(a$errored)
    testthat::expect_length(a$results$effectPages$items,0)
    testthat::expect_equal(a$results$effects$rowCount,0)
    testthat::expect_identical(a$results$reproduction$content,"")
    pl_bulk_change(a,list(generalPaths=original));a$run()
    testthat::expect_false(a$errored)
    testthat::expect_identical(pl_bulk_flatten(a$results$effectPages),before)
})
