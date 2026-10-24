pl_logistic_example<-function() {
    set.seed(291)
    x<-rnorm(320);w<-rnorm(320)
    X<-cbind("(Intercept)"=1,w=w,x=x,"w:x"=w*x)
    list(X=X,y=rbinom(length(x),1,plogis(-.2+.5*x+.2*w+.25*x*w)))
}

testthat::test_that("logistic likelihood, original-unit coefficients and covariance agree with independent glm",{
    d<-pl_logistic_example()
    independent<-stats::glm.fit(d$X,d$y,family=stats::binomial(),control=stats::glm.control(epsilon=1e-12,maxit=100))
    probability<-as.numeric(independent$fitted.values)
    information<-crossprod(d$X,d$X*(probability*(1-probability)))
    bread<-solve(information)
    ll<-sum(dbinom(d$y,1,probability,log=TRUE));null<-sum(dbinom(d$y,1,mean(d$y),log=TRUE))
    for(robust in c(FALSE,TRUE)) {
        f<-pl_fit(d$X,d$y,robust=robust,family="logistic")
        leverage<-probability*(1-probability)*rowSums((d$X%*%bread)*d$X)
        scores<-(d$y-probability)/(1-leverage)
        covariance<-if(robust)bread%*%crossprod(d$X,d$X*scores^2)%*%bread else bread
        testthat::expect_equal(unname(f$coefficients),unname(independent$coefficients),tolerance=1e-9)
        testthat::expect_equal(f$fitted,probability,tolerance=1e-10)
        testthat::expect_equal(unname(f$covariance),unname(covariance),tolerance=1e-9)
        testthat::expect_equal(unname(f$se),unname(sqrt(diag(covariance))),tolerance=1e-9)
        testthat::expect_equal(unname(f$statistic),unname(f$coefficients/f$se),tolerance=1e-12)
        testthat::expect_equal(unname(f$p),unname(2*pnorm(-abs(f$coefficients/f$se))),tolerance=1e-12)
        testthat::expect_equal(f$lower,f$coefficients-qnorm(.975)*f$se,tolerance=1e-12)
        testthat::expect_equal(f$upper,f$coefficients+qnorm(.975)*f$se,tolerance=1e-12)
        testthat::expect_equal(f$log_likelihood,ll,tolerance=1e-10)
        testthat::expect_equal(f$null_log_likelihood,null,tolerance=1e-10)
        testthat::expect_equal(f$pseudo_R2,1-ll/null,tolerance=1e-10)
        testthat::expect_equal(f$AIC,-2*ll+2*ncol(d$X),tolerance=1e-10)
        testthat::expect_equal(f$BIC,-2*ll+log(length(d$y))*ncol(d$X),tolerance=1e-10)
        testthat::expect_equal(f$model_test$statistic,2*(ll-null),tolerance=1e-10)
        testthat::expect_equal(f$model_test$p,pchisq(2*(ll-null),ncol(d$X)-1,lower.tail=FALSE),tolerance=1e-10)
        testthat::expect_identical(f$reference,"normal z")
        testthat::expect_identical(f$model_test$reference,"likelihood-ratio chi-square (model-based)")
        testthat::expect_null(f$r_squared);testthat::expect_null(f$rmse)
        testthat::expect_true(f$convergence$converged)
        testthat::expect_match(f$separation_check,"linear-program")
    }
})

testthat::test_that("logistic fitting preserves predictions and inference after a change of measurement units",{
    d<-pl_logistic_example();multipliers<-c(1,1e3,1e-4,.1)
    changed<-sweep(d$X,2,multipliers,"*")
    for(robust in c(FALSE,TRUE)) {
        a<-pl_fit(d$X,d$y,robust=robust,family="logistic")
        b<-pl_fit(changed,d$y,robust=robust,family="logistic")
        testthat::expect_equal(unname(a$coefficients),unname(b$coefficients*multipliers),tolerance=1e-9)
        testthat::expect_equal(unname(a$covariance),unname(b$covariance*outer(multipliers,multipliers)),tolerance=1e-9)
        testthat::expect_equal(a$fitted,b$fitted,tolerance=1e-10)
        testthat::expect_equal(a$p,b$p,tolerance=1e-10)
        testthat::expect_equal(a$log_likelihood,b$log_likelihood,tolerance=1e-10)
        testthat::expect_equal(pl_fit(changed,d$y,coefficients_only=TRUE,family="logistic"),b$coefficients,tolerance=1e-10)
    }
})

testthat::test_that("signed-margin LP distinguishes overlap, complete separation and quasi separation",{
    x<-seq(-2,2,length.out=80);w<-sin(seq_along(x))
    for(quasi in c(FALSE,TRUE)) {
        xx<-x;y<-as.numeric(x>0)
        if(quasi) { xx[37:44]<-0;y[37:44]<-rep(c(0,1),4) }
        X<-cbind(1,xx,w,xx*w)
        testthat::expect_true(pl_logistic_separation(sweep(X,2,apply(abs(X),2,max),"/")*(2*y-1)))
        testthat::expect_error(pl_fit(X,y,family="logistic"),"SEPARATION")
    }
    # Every design point appears in both outcome classes, establishing overlap.
    X<-rbind(cbind(1,x,w),cbind(1,x,w));y<-rep(c(0,1),each=80)
    testthat::expect_false(pl_logistic_separation(sweep(X,2,apply(abs(X),2,max),"/")*(2*y-1)))
    testthat::expect_equal(unname(pl_fit(X,y,family="logistic")$coefficients),rep(0,3),tolerance=1e-10)
    testthat::expect_error(pl_logistic_separation(X*(2*y-1),max_iterations=0L),"NONCONVERGENCE")
})

testthat::test_that("separation decisions carry independently checkable primal and dual certificates",{
    x<-seq(-2,2,length.out=80);levels<-rep(1:4,20)
    X<-cbind(1,x,as.numeric(levels==2),as.numeric(levels==3),as.numeric(levels==4))
    designs<-list(list(X=X,y=as.numeric(levels==4),separated=TRUE),
        list(X=rbind(X,X),y=rep(c(0,1),each=nrow(X)),separated=FALSE))
    for(case in designs)for(unit in list(rep(1,ncol(X)),c(1,1e-4,1e3,.2,4))) {
        changed<-sweep(case$X,2,unit,"*")
        signed<-sweep(changed,2,apply(abs(changed),2,max),"/")*(2*case$y-1)
        certificate<-pl_logistic_separation(signed,diagnostics=TRUE)
        testthat::expect_identical(certificate$separated,case$separated)
        testthat::expect_true(certificate$primal_feasible)
        testthat::expect_true(certificate$dual_feasible)
        testthat::expect_equal(certificate$margins,as.numeric(signed%*%certificate$direction),tolerance=1e-12)
        constraints<-rbind(cbind(-signed,signed),diag(2*ncol(X)))
        objective<-c(colMeans(signed),-colMeans(signed))
        testthat::expect_gte(min(certificate$dual_multipliers),-1e-12)
        testthat::expect_gte(min(as.numeric(crossprod(constraints,certificate$dual_multipliers))-objective),-1e-12)
        testthat::expect_equal(certificate$objective,certificate$dual_objective,tolerance=1e-12)
        if(case$separated) {
            testthat::expect_gte(min(certificate$margins/mean(certificate$margins)),-1e-7)
            testthat::expect_equal(mean(certificate$margins/mean(certificate$margins)),1,tolerance=1e-12)
        } else testthat::expect_equal(certificate$dual_objective,0,tolerance=1e-12)
    }
})

testthat::test_that("near-boundary separation certificates fail conservatively instead of tolerating contradictory cases",{
    make_overlap<-function(gap) {
        x<-c(seq(-2,-.1,length.out=35),-gap,-gap,gap,gap,seq(.1,2,length.out=35))
        y<-as.numeric(x>0);y[36:39]<-c(0,1,0,1)
        X<-cbind(1,x)
        sweep(X,2,apply(abs(X),2,max),"/")*(2*y-1)
    }
    # The duplicated middle design points have opposing labels. At 1e-8 a
    # refined zero-objective dual certificate establishes overlap exactly.
    testthat::expect_false(pl_logistic_separation(make_overlap(1e-8)))
    # At smaller gaps the pivot tolerance cannot certify its candidate; these
    # cases must be reported as a numerical failure, never as separation.
    for(gap in c(1e-10,1e-12))
        testthat::expect_error(pl_logistic_separation(make_overlap(gap)),"NONCONVERGENCE")
    # Opposite signed columns with a tiny positive common margin do separate.
    # A collapsed reduced cost cannot be used as an overlap certificate.
    weak<-rbind(c(1,-1+1e-12),c(-1,1+1e-12))
    testthat::expect_error(pl_logistic_separation(weak),"NONCONVERGENCE")
})

testthat::test_that("separation tableau work is bounded without accepting an exhausted certificate",{
    d<-pl_logistic_example();scaled<-sweep(d$X,2,apply(abs(d$X),2,max),"/")
    signed<-scaled*(2*d$y-1)
    certificate<-pl_logistic_separation(signed,diagnostics=TRUE)
    testthat::expect_false(certificate$separated)
    testthat::expect_lte(certificate$work_upper_bound,certificate$work_budget)
    testthat::expect_equal(certificate$work_upper_bound,certificate$tableau_cells*certificate$iterations)
    testthat::expect_error(pl_logistic_separation(signed,max_work=certificate$tableau_cells-1),"NONCONVERGENCE.*tableau work budget")
    # A budget allowing only one iteration must also fail if a later pivot or
    # optimality certificate is still needed; no partial result is accepted.
    testthat::expect_error(pl_logistic_separation(signed,max_work=certificate$tableau_cells),"NONCONVERGENCE.*tableau work budget")
    repeated<-signed[rep(seq_len(nrow(signed)),4),,drop=FALSE]
    tall<-pl_logistic_separation(repeated,diagnostics=TRUE)
    testthat::expect_false(tall$separated)
    testthat::expect_lt(tall$iteration_limit,certificate$iteration_limit)
    testthat::expect_lte(tall$work_upper_bound,tall$work_budget)
    x<-seq(-2,2,length.out=80)
    separated<-cbind(1,x)*(2*as.numeric(x>0)-1)
    testthat::expect_true(pl_logistic_separation(separated))
    x[37:44]<-0;y<-as.numeric(x>0);y[37:44]<-rep(c(0,1),4)
    testthat::expect_true(pl_logistic_separation(cbind(1,x)*(2*y-1)))
})

testthat::test_that("logistic failure contracts and equation family dispatch preserve legacy linear calls",{
    d<-pl_logistic_example()
    testthat::expect_error(pl_fit(d$X,rep(1,length(d$y)),family="logistic"),"INVALID_OPTIONS")
    testthat::expect_error(pl_fit(d$X,d$y+.1,family="logistic"),"INVALID_OPTIONS")
    testthat::expect_error(pl_fit(cbind(d$X,d$X[,2]),d$y,family="logistic"),"SINGULAR_DESIGN")
    testthat::expect_error(pl_fit(d$X[1:6,],d$y[1:6],family="logistic"),"SINGULAR_DESIGN")
    testthat::expect_error(pl_fit(d$X,d$y,family="unknown"),"INVALID_OPTIONS")
    equations<-list(y=list(design=d$X,y=d$y,family="logistic"),m=list(design=d$X,y=seq_along(d$y)))
    fit<-fit_equations(equations,robust=FALSE)
    testthat::expect_identical(fit$y$fit$family,"logistic")
    testthat::expect_equal(fit$m$fit,pl_fit(d$X,seq_along(d$y),FALSE,.05,FALSE))
    testthat::expect_equal(pl_fit(d$X,seq_along(d$y),FALSE,.05,TRUE),fit$m$fit$coefficients)
})
