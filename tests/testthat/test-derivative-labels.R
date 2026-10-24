pl_dl_fixture<-function(name) {
    root<-getOption("pathlab.root")
    directory<-if(!is.null(root))file.path(root,"modules/pathlab/tests/fixtures/native-general") else testthat::test_path("..","fixtures","native-general")
    q<-jsonlite::fromJSON(file.path(directory,paste0(name,"-request.json")),simplifyVector=FALSE)
    data<-read.csv(file.path(directory,paste0(name,".csv")))
    indices<-as.matrix(read.csv(file.path(directory,paste0(name,"-indices.csv")),header=FALSE))
    pathlab_fit(data,q$graph,q$settings,draws=200,seed=q$seed,robust=FALSE,indices=indices)
}

testthat::test_that("joint native index labels identify W Z and both contrast derivatives",{
    result<-pl_dl_fixture("joint3_two");spec<-result$graph;rows<-result$estimands
    lookup<-setNames(rows,vapply(rows,`[[`,"","id"))
    indices<-Filter(function(r)length(r$moderator_ids)>0,rows)
    shown<-lapply(indices,function(r)pl_effect_values(spec,r,lookup))
    # This was previously ambiguous: W and Z indices with the same route,
    # index kind and probe values had identical visible text.
    visible<-vapply(shown,function(r)paste(r$kind,r$route,r$condition,sep="|"),"")
    testthat::expect_equal(anyDuplicated(visible),0L)
    first<-Filter(function(r)length(r$path_ids)>0 && length(r$moderator_ids)==1 && identical(unlist(r$moderator_ids),"w") && length(r$condition)>0,indices)[[1]]
    second<-Filter(function(r)identical(r$kind,first$kind) && identical(r$path_ids,first$path_ids) && identical(r$condition,first$condition) && identical(unlist(r$moderator_ids),"z"),indices)[[1]]
    testthat::expect_match(pl_estimand_label(spec,first),"[d/d w]",fixed=TRUE)
    testthat::expect_match(pl_estimand_label(spec,second),"[d/d z]",fixed=TRUE)
    testthat::expect_false(identical(pl_estimand_label(spec,first),pl_estimand_label(spec,second)))
    mixed<-Filter(function(r)length(r$moderator_ids)==2,indices)[[1]]
    testthat::expect_match(pl_estimand_label(spec,mixed),"[d2/(d w d z)]",fixed=TRUE)
    total<-Filter(function(r)!length(r$path_ids) && identical(unlist(r$moderator_ids),"w"),indices)[[1]]
    testthat::expect_identical(pl_estimand_label(spec,total),"Total indirect [d/d w]")
    contrasts<-Filter(function(r)r$kind=="route_derivative_difference",rows)
    for(row in contrasts) {
        a<-lookup[[row$effect_a]];b<-lookup[[row$effect_b]]
        ids<-unlist(a$moderator_ids,use.names=FALSE)
        token<-if(length(ids)==1)paste0("[d/d ",ids,"]") else paste0("[d",length(ids),"/(",paste("d",ids,collapse=" "),")]")
        expected<-paste(paste0(pl_path_label(spec,a$path_ids)," ",token),"minus",paste0(pl_path_label(spec,b$path_ids)," ",token))
        testthat::expect_identical(pl_effect_values(spec,row,lookup)$route,expected)
    }
    ordinary<-Filter(function(r)r$kind=="specific_indirect",rows)[[1]]
    testthat::expect_identical(pl_estimand_label(spec,ordinary),pl_path_label(spec,ordinary$path_ids))
})

testthat::test_that("three-moderator native labels retain the full mixed derivative identity",{
    result<-pl_dl_fixture("separate3_three");indices<-Filter(function(r)length(r$moderator_ids)==3,result$estimands)
    testthat::expect_true(length(indices)>0)
    for(row in indices)testthat::expect_match(pl_estimand_label(result$graph,row),"[d3/(d w d z d v)]",fixed=TRUE)
    # Use assigned column names rather than the internal canonical role IDs.
    renamed<-result$graph
    for(i in seq_along(renamed$nodes))if(renamed$nodes[[i]]$id %in% c("w","z","v"))renamed$nodes[[i]]$column<-c(w="First moderator",z="Second moderator",v="Third moderator")[[renamed$nodes[[i]]$id]]
    testthat::expect_match(pl_estimand_label(renamed,indices[[1]]),"[d3/(d First moderator d Second moderator d Third moderator)]",fixed=TRUE)
})
