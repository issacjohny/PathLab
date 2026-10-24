fixture<-if(!is.null(getOption("pathlab.root")))file.path(getOption("pathlab.root"),"modules/pathlab/tests/fixtures") else testthat::test_path("..","fixtures")
request<-jsonlite::fromJSON(file.path(fixture,"first_stage-request.json"),simplifyVector=FALSE)
data<-read.csv(file.path(fixture,"first_stage.csv"),check.names=FALSE)

testthat::test_that("explicit arrows, IDs, labels and equations are preserved",{
    m<-validate_spec(request$graph,request$settings,names(data)); s<-prepare_sample(data,m); e<-compile_equations(m,s)
    testthat::expect_identical(m$spec,request$graph)
    testthat::expect_identical(names(e$m1$terms),c("(Intercept)","w","x","w:x"))
    testthat::expect_identical(names(e$m2$terms),c("(Intercept)","m1","x"))
    testthat::expect_identical(names(e$y$terms),c("(Intercept)","m1","m2","x"))
    testthat::expect_equal(e$m1$design[,"w:x"],data$w*data$x)
    testthat::expect_match(e$m1$equation,"w \\* x")
    reordered<-request$graph; reordered$nodes<-rev(reordered$nodes); reordered$edges<-rev(reordered$edges)
    ee<-compile_equations(validate_spec(reordered,request$settings,names(data)),s)
    testthat::expect_equal(lapply(ee,`[[`,"design"),lapply(e,`[[`,"design"))
})

testthat::test_that("graph errors and unsupported options are rejected",{
    g<-request$graph; g$edges[[length(g$edges)+1]]<-list(id="cycle",source="m2",target="m1")
    testthat::expect_error(validate_spec(g,request$settings,names(data)),"INVALID_MODEL")
    g<-request$graph; g$edges<-Filter(function(e)e$id!="p_w_m1",g$edges)
    testthat::expect_error(validate_spec(g,request$settings,names(data)),"INVALID_MODEL")
    g<-request$graph; g$nodes[[1]]$unknown<-TRUE
    testthat::expect_error(validate_spec(g,request$settings,names(data)),"INVALID_MODEL")
    opt<-request$settings; opt$outcome_family<-"logistic"
    testthat::expect_error(prepare_sample(data,validate_spec(request$graph,opt,names(data))),"INVALID_OPTIONS")
    opt<-request$settings; opt$categorical<-list(list(node_id="x",levels=c(0,1),reference=0))
    testthat::expect_error(prepare_sample(data,validate_spec(request$graph,opt,names(data))),"INVALID_OPTIONS")
    g<-request$graph; g$nodes[[1]]$measurement_scale<-"nominal"
    testthat::expect_error(validate_spec(g,request$settings,names(data)),"INVALID_OPTIONS")
    opt<-request$settings; opt$probes<-list(list(node_id="w",values=c(0,0)))
    testthat::expect_error(validate_spec(request$graph,opt,names(data)),"INVALID_OPTIONS")
    opt<-request$settings; opt$unknown<-TRUE
    testthat::expect_error(validate_spec(request$graph,opt,names(data)),"INVALID_OPTIONS")
    testthat::expect_error(pathlab_fit(data,request$graph,draws=10),"INVALID_OPTIONS")
    testthat::expect_error(pathlab_fit(data,request$graph,seed=NA_real_),"INVALID_OPTIONS")
})

testthat::test_that("one fixed sample reports missing source rows and rejects invalid input",{
    m<-validate_spec(request$graph,request$settings,names(data)); d<-data
    d$m1[1]<-NA; d$y[3]<-NA; s<-prepare_sample(d,m)
    testthat::expect_equal(s$n,98); testthat::expect_equal(s$included_rows,setdiff(1:100,c(1,3)))
    testthat::expect_equal(vapply(s$exclusions,`[[`,0L,"source_row"),c(1L,3L))
    d<-data; d$x<-factor(d$x)
    testthat::expect_error(prepare_sample(d,m),"NUMERIC_TYPE_MISMATCH")
    d<-data; d$x[1]<-Inf
    testthat::expect_error(prepare_sample(d,m),"INVALID_DATA")
    d<-data; d$x<-1
    testthat::expect_error(prepare_sample(d,m),"CONSTANT_VARIABLE")
    testthat::expect_error(prepare_sample(data[FALSE,],m),"INSUFFICIENT_DATA")
    testthat::expect_error(pl_fit(cbind(1,data$x,data$x),data$y),"SINGULAR_DESIGN")
    testthat::expect_error(pl_fit(cbind(1,1:4),1:4),"SINGULAR_DESIGN")
})

testthat::test_that("bootstrap failure accounting, exact threshold and RNG isolation hold",{
    idx<-as.matrix(read.csv(file.path(fixture,"first_stage-indices.csv"),header=FALSE))
    idx[1:10,]<-1L
    set.seed(77); previous<-.Random.seed; kind<-RNGkind()
    r<-pathlab_fit(data,request$graph,request$settings,draws=200,indices=idx)
    testthat::expect_identical(r$provenance$module,sub("^pathlab-r/","",r$engine))
    testthat::expect_equal(r$bootstrap$successful,190)
    testthat::expect_equal(r$bootstrap$failed,10)
    testthat::expect_equal(r$bootstrap$failure_reasons$SINGULAR_DESIGN,10)
    testthat::expect_equal(.Random.seed,previous); testthat::expect_identical(RNGkind(),kind)
    idx[11,]<-1L
    testthat::expect_error(pathlab_fit(data,request$graph,request$settings,draws=200,indices=idx),"BOOTSTRAP_FAILURE")
    testthat::expect_error(pathlab_fit(data,request$graph,request$settings,draws=200,indices=matrix(0,200,100)),"INVALID_OPTIONS")
    a<-pathlab_fit(data,request$graph,request$settings,draws=200,seed=7)
    testthat::expect_identical(.Random.seed,previous); testthat::expect_identical(RNGkind(),kind)
    b<-pathlab_fit(data,request$graph,request$settings,draws=200,seed=7)
    testthat::expect_identical(a$estimands,b$estimands)
    testthat::expect_identical(a$sample$probes,b$sample$probes)
})

testthat::test_that("known route products and varying derivatives are algebraically correct",{
    q<-jsonlite::fromJSON(file.path(fixture,"repeated_w-request.json"),simplifyVector=FALSE)
    m<-validate_spec(q$graph,q$settings,names(data)); s<-prepare_sample(data,m); eq<-fit_equations(compile_equations(m,s))
    beta<-lapply(eq,function(e)setNames(rep(0,length(e$terms)),names(e$terms)))
    beta$m1[c("x","w:x")]<-c(2,.5); beta$m2[c("m1","m1:w")]<-c(3,.4); beta$y[c("m2","m2:w")]<-c(4,.3)
    path<-c("p_x_m1","p_m1_m2","p_m2_y")
    for(w in c(-1,0,2)) {
        value<-(2+.5*w)*(3+.4*w)*(4+.3*w)
        derivative<-.5*(3+.4*w)*(4+.3*w)+(2+.5*w)*.4*(4+.3*w)+(2+.5*w)*(3+.4*w)*.3
        testthat::expect_equal(pl_route(path,m,eq,beta,list(w=w)),value)
        testthat::expect_equal(pl_route(path,m,eq,beta,list(w=w),"w"),derivative)
    }
    rows<-evaluate_estimands(m,s,eq,beta)
    serial<-Filter(function(r)setequal(r$path_ids,path) && r$kind=="conditional_derivative",rows)
    testthat::expect_length(serial,3)
    testthat::expect_false(any(vapply(rows,function(r)setequal(r$path_ids,path) && r$kind=="linear_index",TRUE)))
})

testthat::test_that("native controls retain every selected arrow and moderation",{
    controls<-list(x="x",m1="m1",m2="m2",y="y",w="w",p_x_m1=TRUE,p_x_m2=TRUE,p_m1_m2=TRUE,p_x_y=TRUE,p_m1_y=TRUE,p_m2_y=TRUE,p_w_m1=TRUE,mod_x_m1=TRUE)
    g<-native_spec(controls)
    testthat::expect_setequal(pl_ids(g$edges),pl_ids(request$graph$edges))
    testthat::expect_equal(g$moderations[[1]]$path_id,"p_x_m1")
    controls$p_x_y<-FALSE; g<-native_spec(controls)
    testthat::expect_false("p_x_y" %in% pl_ids(g$edges))
    controls$w<-NULL
    testthat::expect_error(native_spec(controls),"INCOMPLETE_MODEL")
})
