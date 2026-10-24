testthat::test_that("singleton-cell rejection explains unit leverage without changing bootstrap fits",{
    # Full rank is insufficient for the original-fit leverage guard: the rare
    # cell's only observation is perfectly fitted regardless of its response.
    X<-cbind('(Intercept)'=1,rare=c(1,rep(0,19)),score=seq(-1,1,length.out=20))
    y<-sin(seq_len(20))
    testthat::expect_equal(qr(X)$rank,3L)
    for(robust in c(FALSE,TRUE)) {
        error<-tryCatch(pl_fit(X,y,robust=robust),error=identity)
        testthat::expect_s3_class(error,'pathlab_error')
        testthat::expect_identical(error$code,'SINGULAR_DESIGN')
        testthat::expect_match(conditionMessage(error),'unit leverage',fixed=TRUE)
        testthat::expect_match(conditionMessage(error),'singleton joint categorical cells',fixed=TRUE)
    }
    testthat::expect_equal(pl_fit(X,y,coefficients_only=TRUE),stats::lm.fit(X,y)$coefficients)
})
