fixture<-if(!is.null(getOption("pathlab.root")))file.path(getOption("pathlab.root"),"modules/pathlab/tests/fixtures") else testthat::test_path("..","fixtures")

testthat::test_that("constant route indices remain in every conditional derivative comparison",{
    q<-jsonlite::fromJSON(file.path(fixture,"repeated_w-request.json"),simplifyVector=FALSE)
    d<-read.csv(file.path(fixture,"repeated_w.csv"))
    m<-validate_spec(q$graph,q$settings,names(d)); s<-prepare_sample(d,m)
    eq<-fit_equations(compile_equations(m,s))
    rows<-evaluate_estimands(m,s,eq)
    constants<-Filter(function(r)r$kind=="linear_index" && r$effect_kind=="specific_indirect",rows)
    varying<-Filter(function(r)r$kind=="conditional_derivative" && r$effect_kind=="specific_indirect",rows)
    differences<-Filter(function(r)r$kind=="route_derivative_difference",rows)
    testthat::expect_length(constants,2)
    testthat::expect_length(varying,3)
    testthat::expect_length(differences,7)
    for(a in varying)for(b in constants) {
        pair<-Filter(function(r)setequal(c(r$effect_a,r$effect_b),c(a$id,b$id)),differences)
        testthat::expect_length(pair,1)
        testthat::expect_equal(pair[[1]]$estimate,if(pair[[1]]$effect_a==a$id)a$estimate-b$estimate else b$estimate-a$estimate,tolerance=1e-12)
    }
    # Removing the nonlinear stage leaves only constant route indices. Their
    # three pairwise comparisons should each appear once across all probes.
    q$graph$moderations<-q$graph$moderations[1]
    m<-validate_spec(q$graph,q$settings,names(d)); s<-prepare_sample(d,m)
    eq<-fit_equations(compile_equations(m,s)); rows<-evaluate_estimands(m,s,eq)
    testthat::expect_equal(sum(vapply(rows,function(r)r$kind=="route_derivative_difference",TRUE)),3)
})

testthat::test_that("valid moderator identifiers cannot collide across derivative subsets",{
    q<-jsonlite::fromJSON(file.path(fixture,"separate_three-request.json"),simplifyVector=FALSE)
    d<-read.csv(file.path(fixture,"separate_three.csv"))
    replacements<-c(q="a_b",w="a",z="b")
    rename<-function(id)if(id %in% names(replacements))unname(replacements[id]) else id
    q$graph$nodes<-lapply(q$graph$nodes,function(n){n$id<-rename(n$id);n})
    q$graph$edges<-lapply(q$graph$edges,function(e){e$source<-rename(e$source);e$target<-rename(e$target);e})
    q$graph$moderations<-lapply(q$graph$moderations,function(m){m$moderator_ids<-lapply(m$moderator_ids,rename);m})
    q$settings$probes<-lapply(q$settings$probes,function(p){p$node_id<-rename(p$node_id);p})
    m<-validate_spec(q$graph,q$settings,names(d));s<-prepare_sample(d,m)
    eq<-fit_equations(compile_equations(m,s));rows<-evaluate_estimands(m,s,eq)
    testthat::expect_false(anyDuplicated(vapply(rows,`[[`,"","id"))>0)
    route<-c("p_x_m1","p_m1_m2","p_m2_y")
    specific<-Filter(function(r)identical(r$effect_kind,"specific_indirect") && setequal(r$path_ids,route),rows)
    single<-Filter(function(r)identical(r$moderator_ids,"a_b"),specific)
    mixed<-Filter(function(r)setequal(r$moderator_ids,c("a","b")),specific)
    testthat::expect_length(single,8)
    testthat::expect_length(mixed,8)
    testthat::expect_length(intersect(vapply(single,`[[`,"","id"),vapply(mixed,`[[`,"","id")),0)
    for(a in c(single,mixed))testthat::expect_equal(a$estimate,pl_route(route,m,eq,lapply(eq,function(e)e$fit$coefficients),a$condition,a$moderator_ids),tolerance=1e-12)
    testthat::expect_identical(pl_derivative_key(c("w","z")),"w_z")
    testthat::expect_identical(pl_derivative_key(c("b","a_b")),pl_derivative_key(c("a_b","b")))
    testthat::expect_false(identical(pl_derivative_key("a_b"),pl_derivative_key(c("a","b"))))
    testthat::expect_false(identical(pl_derivative_key(c("a_b","c")),pl_derivative_key(c("a","b_c"))))
})
