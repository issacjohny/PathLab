pl_cov_controls<-function()list(x="x",m1="m1",m2="m2",y="y",w="w",p_x_m1=TRUE,p_x_m2=TRUE,p_m1_m2=TRUE,p_x_y=TRUE,p_m1_y=TRUE,p_m2_y=TRUE,p_w_m1=TRUE,mod_x_m1=TRUE,covM1="c1",covM2="c2",covY=c("c2","c1"))
pl_cov_data<-function() {
    root<-getOption("pathlab.root")
    path<-if(!is.null(root))file.path(root,"modules/pathlab/tests/fixtures/first_stage.csv") else testthat::test_path("..","fixtures","first_stage.csv")
    d<-read.csv(path,check.names=FALSE)
    d$c1<-sin(seq_len(nrow(d)))/7+.2*d$x
    d$c2<-cos(seq_len(nrow(d))*.47)+.1*d$m2
    d
}
pl_cov_settings<-list(outcome_family="linear",categorical=list(),probes=list())

testthat::test_that("native covariates retain only explicit equation assignments",{
    o<-pl_cov_controls();g<-native_spec(o)
    covs<-Filter(function(n)n$role=="covariate",g$nodes)
    testthat::expect_identical(vapply(covs,`[[`,"","id"),c("cov1","cov2"))
    testthat::expect_identical(vapply(covs,`[[`,"","column"),c("c1","c2"))
    edges<-Filter(function(e)startsWith(e$source,"cov"),g$edges)
    testthat::expect_identical(pl_ids(edges),c("p_cov1_m1","p_cov2_m2","p_cov1_y","p_cov2_y"))
    testthat::expect_false(any(vapply(edges,function(e)e$source=="cov1"&&e$target=="m2",TRUE)))
    o$covY<-rev(o$covY)
    testthat::expect_identical(native_spec(o),g)
    d<-pl_cov_data();m<-validate_spec(g,pl_cov_settings,names(d));s<-prepare_sample(d,m)
    original<-compile_equations(m,s)
    o$covM1<-NULL
    revised<-compile_equations(validate_spec(native_spec(o),pl_cov_settings,names(d)),s)
    testthat::expect_setequal(setdiff(names(original$m1$terms),names(revised$m1$terms)),"cov1")
    testthat::expect_identical(original$m2,revised$m2)
    testthat::expect_identical(original$y,revised$y)
})

testthat::test_that("native covariate validation prevents missing roles and collisions",{
    o<-pl_cov_controls();o$m1<-NULL
    testthat::expect_error(native_spec(o),"Covariates assigned to M1 require")
    for(column in c("x","m1","m2","y","w")) {
        o<-pl_cov_controls();o$covM1<-column
        testthat::expect_error(native_spec(o),"covariate column cannot also")
    }
    o<-pl_cov_controls();o$covY<-c("c1","c1")
    testthat::expect_error(native_spec(o),"distinct column names")
    o<-pl_cov_controls();o$covY<-NA_character_
    testthat::expect_error(native_spec(o),"distinct column names")
    o<-pl_cov_controls();o$covY<-1:2
    testthat::expect_error(native_spec(o),"distinct column names")
    o<-pl_cov_controls();o$covY<-paste0("c",seq_len(21))
    testthat::expect_error(native_spec(o),"At most twenty")
})

testthat::test_that("covariate equations agree with handwritten lm and HC3 algebra",{
    o<-pl_cov_controls();d<-pl_cov_data();g<-native_spec(o)
    m<-validate_spec(g,pl_cov_settings,names(d));s<-prepare_sample(d,m)
    equations<-compile_equations(m,s)
    reference<-list(m1=stats::lm(m1~c1+w+x+w:x,d),m2=stats::lm(m2~c2+m1+x,d),y=stats::lm(y~c1+c2+m1+m2+x,d))
    for(robust in c(FALSE,TRUE)) {
        fitted<-fit_equations(equations,robust)
        for(target in names(reference)) {
            independent<-reference[[target]];X<-stats::model.matrix(independent)
            testthat::expect_equal(as.vector(equations[[target]]$design),as.vector(X),tolerance=1e-12)
            f<-fitted[[target]]$fit
            testthat::expect_equal(unname(f$coefficients),unname(stats::coef(independent)),tolerance=1e-10)
            bread<-solve(crossprod(X));h<-rowSums((X%*%bread)*X)
            covariance<-if(robust)bread%*%crossprod(X,X*(stats::resid(independent)/(1-h))^2)%*%bread else stats::vcov(independent)
            testthat::expect_equal(unname(f$covariance),unname(covariance),tolerance=1e-10)
            testthat::expect_equal(f$df_residual,stats::df.residual(independent))
        }
    }
    o$covM2<-NULL;o$covY<-NULL
    d$c1[c(2,9)]<-NA_real_
    model<-validate_spec(native_spec(o),pl_cov_settings,names(d));sample<-prepare_sample(d,model)
    all_equations<-compile_equations(model,sample)
    testthat::expect_equal(sample$n,98)
    testthat::expect_true(all(vapply(all_equations,function(e)nrow(e$design),0L)==98))
    testthat::expect_equal(sample$included_rows,setdiff(seq_len(100),c(2,9)))
})

testthat::test_that("generated native covariate options round-trip as variable lists",{
    testthat::skip_if_not_installed("jmvcore")
    options_class<-if(exists("pathsOptions",inherits=TRUE))get("pathsOptions",inherits=TRUE) else getFromNamespace("pathsOptions","pathlab")
    o<-options_class$new(x="x",m1="m1",m2="m2",y="y",w="w",covM1="c1",covM2="c2",covY=c("c2","c1"))
    testthat::expect_identical(o$covM1,"c1")
    testthat::expect_identical(o$covY,c("c2","c1"))
    required<-unlist(o$varsRequired,use.names=FALSE)
    testthat::expect_true(all(c("x","m1","m2","y","w","c1","c2") %in% required))
})

testthat::test_that("public native function accepts omitted optional covariates and mediators",{
    testthat::skip_if_not_installed("jmvcore")
    public_paths<-if(exists("paths",inherits=TRUE))get("paths",inherits=TRUE) else getFromNamespace("paths","pathlab")
    for(name in c("m1","m2","w","covM1","covM2","covY"))testthat::expect_null(formals(public_paths)[[name]])
    d<-pl_cov_data()
    serial<-public_paths(data=d,x="x",m1="m1",m2="m2",y="y",w="w",draws=200,robust=FALSE)
    testthat::expect_equal(serial$equations$rowCount,3)
    testthat::expect_equal(serial$estimates$rowCount,11)
    ordinary<-public_paths(data=d,x="x",y="y",w="w",p_x_m1=FALSE,p_x_m2=FALSE,p_m1_m2=FALSE,p_m1_y=FALSE,p_m2_y=FALSE,p_w_m1=FALSE,mod_x_m1=FALSE,p_w_y=TRUE,mod_x_y=TRUE,draws=200,robust=FALSE)
    testthat::expect_equal(ordinary$equations$rowCount,1)
    testthat::expect_equal(ordinary$estimates$rowCount,4)
})
