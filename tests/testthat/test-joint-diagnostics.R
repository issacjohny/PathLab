pl_joint_fixture <- function(name) {
    root<-getOption("pathlab.root")
    if(!is.null(root))file.path(root,"modules/pathlab/tests/fixtures",name) else testthat::test_path("..","fixtures",name)
}
pl_joint_condition_key <- function(c)if(length(c))paste(sort(paste(names(c),sprintf("%.12g",unlist(c)),sep="=")),collapse="|") else ""
for(case in c("joint_two","separate_three"))for(robust in c(FALSE,TRUE))local({
    name<-case;hc3<-robust
    testthat::test_that(paste(name,hc3,"complete mean-conditioned slopes and conditional JN slices match Python"),{
        q<-jsonlite::fromJSON(pl_joint_fixture(paste0(name,"-request.json")),simplifyVector=FALSE)
        d<-read.csv(pl_joint_fixture(paste0(name,".csv")),check.names=FALSE)
        py<-jsonlite::fromJSON(pl_joint_fixture(paste0(name,"-python.json")),simplifyVector=FALSE)[[tolower(as.character(hc3))]]
        m<-validate_spec(q$graph,q$settings,names(d));s<-prepare_sample(d,m);eq<-fit_equations(compile_equations(m,s),hc3)
        result<-pl_slopes(m,s,eq,.05)
        testthat::expect_length(result$slopes,length(py$slopes))
        for(p in py$slopes) {
            rows<-Filter(function(r)r$path_id==p$path_id && pl_joint_condition_key(r$condition)==pl_joint_condition_key(p$condition),result$slopes)
            testthat::expect_length(rows,1)
            for(stat in c("estimate","se","lower","upper","p"))testthat::expect_equal(rows[[1]][[stat]],p[[stat]],tolerance=1e-8)
            testthat::expect_match(rows[[1]]$conditioning_note,"complete-case means",fixed=TRUE)
        }
        computed<-Filter(function(p)p$status=="computed",py$johnson_neyman)
        testthat::expect_length(result$johnson_neyman,length(computed))
        for(p in computed) {
            rows<-Filter(function(r)r$path_id==p$path_id && r$moderator_id==p$moderator_id && pl_joint_condition_key(r$other_conditions)==pl_joint_condition_key(p$other_conditions),result$johnson_neyman)
            testthat::expect_length(rows,1);r<-rows[[1]]
            testthat::expect_identical(r$status,"available")
            testthat::expect_equal(r$boundaries,as.numeric(unlist(p$boundaries)),tolerance=1e-8)
            testthat::expect_equal(r$observed_range,as.numeric(unlist(p$observed_range)),tolerance=1e-12)
            testthat::expect_length(r$regions,length(p$regions))
            for(k in seq_along(p$regions)) {
                testthat::expect_equal(c(r$regions[[k]]$lower,r$regions[[k]]$upper),c(p$regions[[k]]$lower,p$regions[[k]]$upper),tolerance=1e-8)
                testthat::expect_identical(r$regions[[k]]$significant,p$regions[[k]]$significant)
            }
        }
        if(name=="joint_two") {
            # Explicit independent model and slope vector. In particular,
            # d(M1)/d(W) retains both X:W and X:W:Z at original-sample mean(X).
            fit<-stats::lm(m1~x+w+z+x:w+x:z+w:z+x:w:z,d)
            X<-stats::model.matrix(fit);bread<-solve(crossprod(X));h<-rowSums((X%*%bread)*X)
            covariance<-if(hc3)bread%*%crossprod(X,X*(stats::resid(fit)/(1-h))^2)%*%bread else stats::vcov(fit)
            w_rows<-Filter(function(r)r$path_id=="p_w_m1",result$slopes)
            testthat::expect_length(w_rows,2)
            for(r in w_rows) {
                v<-setNames(rep(0,length(stats::coef(fit))),names(stats::coef(fit)))
                v[c("w","x:w","w:z","x:w:z")]<-c(1,mean(d$x),r$condition$z,mean(d$x)*r$condition$z)
                testthat::expect_equal(r$estimate,sum(v*stats::coef(fit)),tolerance=1e-10)
                testthat::expect_equal(r$se,sqrt(as.numeric(t(v)%*%covariance%*%v)),tolerance=1e-10)
                testthat::expect_equal(r$held_at_means$x,mean(d$x),tolerance=1e-12)
                testthat::expect_false("w" %in% names(r$condition))
            }
        }
    })
})

testthat::test_that("JN handles linear and constant significance polynomials",{
    q<-jsonlite::fromJSON(pl_joint_fixture("ordinary_moderation-request.json"),simplifyVector=FALSE)
    d<-read.csv(pl_joint_fixture("ordinary_moderation.csv"))
    m<-validate_spec(q$graph,q$settings,names(d));s<-prepare_sample(d,m);eq<-fit_equations(compile_equations(m,s))
    f<-eq$y$fit;f$reference<-"normal";f$coefficients[]<-0;f$covariance[,]<-0
    dimnames(f$covariance)<-list(names(f$coefficients),names(f$coefficients))
    f$coefficients[c("x","w:x")]<-c(.2,qnorm(.975)*.1)
    diag(f$covariance)<-.01;f$covariance["x","x"]<-.04;eq$y$fit<-f
    linear<-pl_slopes(m,s,eq,.05)$johnson_neyman[[1]]
    expected<--linear$polynomial[1]/linear$polynomial[2]
    testthat::expect_length(linear$roots,1)
    testthat::expect_equal(linear$roots,expected,tolerance=1e-10)
    f$coefficients["w:x"]<-0;f$covariance["w:x","w:x"]<-0;f$covariance["x","x"]<-.01;eq$y$fit<-f
    constant<-pl_slopes(m,s,eq,.05)$johnson_neyman[[1]]
    testthat::expect_length(constant$roots,0)
    testthat::expect_length(constant$regions,1)
    testthat::expect_true(constant$regions[[1]]$significant)
})
