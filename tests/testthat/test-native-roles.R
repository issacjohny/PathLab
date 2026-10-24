pl_role_controls <- function() {
    edges<-c("x_m1","x_m2","m1_m2","x_y","m1_y","m2_y","w_m1","w_m2","w_y")
    mods<-edges[seq_len(6)]
    c(list(x="x",m1=NULL,m2=NULL,y="y",w=NULL),setNames(as.list(rep(FALSE,length(edges))),paste0("p_",edges)),setNames(as.list(rep(FALSE,length(mods))),paste0("mod_",mods)))
}
pl_role_data <- function() {
    root<-getOption("pathlab.root")
    file<-if(!is.null(root)) file.path(root,"modules/pathlab/tests/fixtures/first_stage.csv") else testthat::test_path("..","fixtures","first_stage.csv")
    read.csv(file,check.names=FALSE)
}

testthat::test_that("native default serial graph retains every selected role and arrow",{
    o<-pl_role_controls(); o$m1<-"m1"; o$m2<-"m2"; o$w<-"w"
    for(name in c("p_x_m1","p_x_m2","p_m1_m2","p_x_y","p_m1_y","p_m2_y","p_w_m1","mod_x_m1"))o[[name]]<-TRUE
    g<-native_spec(o)
    testthat::expect_identical(pl_ids(g$nodes),c("x","m1","m2","y","w"))
    testthat::expect_identical(pl_ids(g$edges),c("p_x_m1","p_x_m2","p_m1_m2","p_x_y","p_m1_y","p_m2_y","p_w_m1"))
    testthat::expect_identical(g$moderations[[1]],list(id="mod_x_m1",path_id="p_x_m1",moderator_ids=list("w")))
})

testthat::test_that("native optional mediators preserve the explicit graph",{
    d<-pl_role_data()
    for(id in c("m1","m2")) {
        o<-pl_role_controls(); o[[id]]<-id
        for(name in c(paste0("p_x_",id),paste0("p_",id,"_y"),"p_x_y"))o[[name]]<-TRUE
        g<-native_spec(o)
        testthat::expect_setequal(pl_ids(g$nodes),c("x",id,"y"))
        testthat::expect_length(g$edges,3)
        testthat::expect_length(g$moderations,0)
        m<-validate_spec(g,list(outcome_family="linear",categorical=list(),probes=list()),names(d))
        testthat::expect_length(m$routes,2)
    }
    o<-pl_role_controls();o$m1<-"m1";o$m2<-"";o$p_x_m1<-TRUE;o$p_m1_y<-TRUE
    testthat::expect_setequal(pl_ids(native_spec(o)$nodes),c("x","m1","y"))
})

testthat::test_that("native ordinary moderation requires no mediator",{
    o<-pl_role_controls();o$w<-"w";o$p_x_y<-TRUE;o$p_w_y<-TRUE;o$mod_x_y<-TRUE
    g<-native_spec(o)
    testthat::expect_identical(pl_ids(g$nodes),c("x","y","w"))
    testthat::expect_identical(pl_ids(g$edges),c("p_x_y","p_w_y"))
    testthat::expect_identical(g$moderations[[1]]$path_id,"p_x_y")
    m<-validate_spec(g,list(outcome_family="linear",categorical=list(),probes=list()),names(pl_role_data()))
    testthat::expect_length(m$routes,1)
})

testthat::test_that("native unassigned roles never silently remove selected arrows",{
    o<-pl_role_controls();o$p_x_m1<-TRUE
    testthat::expect_error(native_spec(o),"Selected arrow X -> M1 requires M1")
    o<-pl_role_controls();o$p_m1_m2<-TRUE
    testthat::expect_error(native_spec(o),"M1, M2")
    o<-pl_role_controls();o$p_w_y<-TRUE
    testthat::expect_error(native_spec(o),"Selected arrow W -> Y requires W")
    o<-pl_role_controls();o$mod_m2_y<-TRUE
    testthat::expect_error(native_spec(o),"Selected moderation.*M2.*W")
    o<-pl_role_controls();o$w<-"w";o$mod_x_y<-TRUE
    testthat::expect_error(native_spec(o),"requires its explicit arrow X -> Y")
    o$p_x_y<-TRUE
    testthat::expect_error(native_spec(o),"main-effect arrow W -> Y")
    o<-pl_role_controls();o$x<-NULL
    testthat::expect_error(native_spec(o),"Select X and Y")
    o<-pl_role_controls();o$m1<-c("m1","m2")
    testthat::expect_error(native_spec(o),"Choose one column for M1")
    o<-pl_role_controls();o$p_x_y<-1
    testthat::expect_error(native_spec(o),"INVALID_OPTIONS")
})

testthat::test_that("native simple mediation and ordinary moderation agree with independent lm",{
    d<-pl_role_data()
    for(id in c("m1","m2")) {
        o<-pl_role_controls();o[[id]]<-id
        for(name in c(paste0("p_x_",id),paste0("p_",id,"_y"),"p_x_y"))o[[name]]<-TRUE
        r<-pathlab_fit(d,native_spec(o),draws=200,robust=FALSE)
        mediator<-stats::lm(stats::reformulate("x",response=id),d)
        outcome<-stats::lm(stats::reformulate(c(id,"x"),response="y"),d)
        testthat::expect_equal(unname(r$equations[[id]]$fit$coefficients),unname(stats::coef(mediator)),tolerance=1e-10)
        testthat::expect_equal(unname(r$equations$y$fit$coefficients),unname(stats::coef(outcome)),tolerance=1e-10)
        indirect<-Filter(function(row)row$kind=="specific_indirect",r$estimands)
        testthat::expect_length(indirect,1)
        testthat::expect_equal(indirect[[1]]$estimate,unname(stats::coef(mediator)["x"]*stats::coef(outcome)[id]),tolerance=1e-10)
    }
    o<-pl_role_controls();o$w<-"w";o$p_x_y<-TRUE;o$p_w_y<-TRUE;o$mod_x_y<-TRUE
    r<-pathlab_fit(d,native_spec(o),draws=200,robust=FALSE)
    reference<-stats::lm(y~w*x,d)
    testthat::expect_equal(unname(r$equations$y$fit$coefficients),unname(stats::coef(reference)),tolerance=1e-10)
    testthat::expect_length(r$equations,1)
    testthat::expect_length(r$slopes,3)
    for(row in r$slopes) testthat::expect_equal(row$estimate,unname(stats::coef(reference)["x"]+row$condition$w*stats::coef(reference)["w:x"]),tolerance=1e-10)
})
