pl_row_default_options<-function()list(modelMode="general",x="x",y="y",generalMediators=c("m1","m2"),generalModerators="w",
    generalPaths=list(list(source=NULL,target=NULL),list(source=NULL,target="M2"),list(source="M1",target="Y"),list(source="M2",target="Y"),list(source=NULL,target="Y"),list(source="W",target=NULL),list(source="W",target="M2"),list(source="W",target="Y")),
    generalModerations=list(list(source=NULL,target=NULL,mod1=NULL,mod2=NULL),list(source=NULL,target="M2",mod1=NULL,mod2=NULL),list(source=NULL,target="Y",mod1=NULL,mod2=NULL)))

testthat::test_that("untouched native row cells resolve to their visible choices",{
    raw<-pl_row_default_options();explicit<-raw
    targets<-c("M1","M2","Y","Y","Y","M1","M2","Y")
    sources<-c("X","X","M1","M2","X","W","W","W")
    explicit$generalPaths<-unname(Map(function(source,target)list(source=source,target=target),sources,targets))
    explicit$generalModerations<-lapply(c("M1","M2","Y"),function(target)list(source="X",target=target,mod1="W",mod2="none"))
    testthat::expect_identical(native_spec(raw),native_spec(explicit))
    testthat::expect_identical(raw$generalPaths[[1]],list(source=NULL,target=NULL))
    g<-native_spec(raw)
    testthat::expect_length(g$edges,8)
    testthat::expect_length(g$moderations,3)
    testthat::expect_identical(g$edges[[1]]$target,"m1")
    testthat::expect_identical(g$edges[[5]]$target,"y")
    testthat::expect_length(validate_spec(g,native_settings(raw,g),c("x","y","m1","m2","w"))$routes,3)
})

testthat::test_that("native default resolution retains strict malformed-row errors",{
    for(row in list(list(source="X"),list(source="X",target=""),list(source="X",target=NA_character_),list(source=1,target="M1"),list(source="X",target="M1",extra="Y"))) {
        o<-pl_row_default_options();o$generalPaths[[1]]<-row
        testthat::expect_error(native_spec(o),"generalPaths row 1")
    }
    o<-pl_row_default_options();o$generalPaths[[2]]$target<-"M3"
    testthat::expect_error(native_spec(o),"M3 has no assigned column")
    o<-pl_row_default_options();o$generalModerations[[2]]$mod1<-"Z"
    testthat::expect_error(native_spec(o),"Z has no assigned column")
    o<-pl_row_default_options();o$generalModerations[[2]]$mod2<-""
    testthat::expect_error(native_spec(o),"generalModerations row 2")
})
