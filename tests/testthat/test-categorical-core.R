pl_cat_core_fixture<-function(joint=TRUE) {
    n<-180L;i<-seq_len(n)
    d<-data.frame(x=rep(c(1,2,3),length.out=n),w=rep(c("alpha","base","omega"),each=3,length.out=n),z=sin(i*.23)+i/n,c=rep(c(10,20),each=9,length.out=n),check.names=FALSE)
    d$m<-1+.7*(d$x==1)-.3*(d$x==3)+.6*(d$w=="alpha")+.2*d$z+.1*(d$c==20)+sin(i*.71)
    d$y<-2+.5*d$m+.4*(d$x==1)+cos(i*.37)
    nodes<-lapply(c("x","m","y","w","z","c"),function(id)list(id=id,column=id,role=switch(id,x="independent",m="mediator",y="dependent",w="moderator",z="moderator",c="covariate"),measurement_scale=if(id %in% c("x","w","c"))"nominal" else "interval"))
    pairs<-list(c("x","m"),c("m","y"),c("x","y"),c("w","m"),c("z","m"),c("c","m"),c("c","y"))
    edges<-lapply(pairs,function(p)list(id=paste0("p_",paste(p,collapse="_")),source=p[1],target=p[2]))
    mods<-list(list(id="xm_w",path_id="p_x_m",moderator_ids=list("w")),list(id="xm_z",path_id="p_x_m",moderator_ids=list("z")))
    if(joint)mods<-c(mods,list(list(id="wm_z",path_id="p_w_m",moderator_ids=list("z")),list(id="xm_wz",path_id="p_x_m",moderator_ids=list("w","z"))))
    settings<-list(outcome_family="linear",categorical=list(list(node_id="x",levels=list(1,2,3),reference=2),list(node_id="w",levels=list("alpha","base","omega"),reference="base"),list(node_id="c",levels=list(10,20),reference=10)),probes=list(list(node_id="z",values=c(-1,1))))
    graph<-list(schema_version=1,nodes=nodes,edges=edges,moderations=mods)
    model<-validate_spec(graph,settings,names(d));sample<-prepare_sample(d,model)
    list(data=d,graph=graph,settings=settings,model=model,sample=sample,equations=compile_equations(model,sample))
}

testthat::test_that("declared middle references retain typed indicator bases and fixed sample counts",{
    f<-pl_cat_core_fixture()
    testthat::expect_identical(f$sample$basis$x,c("x@0","x@2"))
    testthat::expect_identical(f$sample$basis$w,c("w@0","w@2"))
    testthat::expect_identical(f$sample$basis$c,"c@1")
    testthat::expect_identical(f$sample$basis$m,"m")
    testthat::expect_identical(f$model$numerical_moderators,"z")
    testthat::expect_identical(f$sample$basis_description[["x@0"]],list(node_id="x",column="x",level=1,reference=2))
    testthat::expect_identical(f$sample$basis_description[["w@2"]],list(node_id="w",column="w",level="omega",reference="base"))
    testthat::expect_identical(f$sample$probes$w,c("alpha","base","omega"))
    testthat::expect_length(f$sample$conditions,6)
    testthat::expect_identical(f$sample$conditions[[1]],list(w="alpha",z=-1))
    testthat::expect_equal(vapply(f$sample$coding_rows[[1]]$counts,`[[`,0,"n"),rep(60,3))
    testthat::expect_identical(f$sample$coding_rows[[2]]$reference,"base")
    testthat::expect_identical(f$sample$extrapolation,"z")
    d<-f$data;d$m[1]<-NA;d$w[2]<-NA
    s<-prepare_sample(d,f$model)
    testthat::expect_identical(s$included_rows,3:180)
    testthat::expect_equal(vapply(s$coding_rows[[1]]$counts,`[[`,0,"n"),c(59,59,60))
    testthat::expect_identical(s$basis,f$sample$basis)
})

testthat::test_that("categorical Cartesian terms equal a separately handwritten indicator matrix",{
    f<-pl_cat_core_fixture();d<-f$data;eq<-f$equations$m
    # Deliberately list every column independently of the graph compiler.
    X<-cbind(1,as.numeric(d$c==20),as.numeric(d$w=="alpha"),as.numeric(d$w=="omega"),as.numeric(d$x==1),as.numeric(d$x==3),d$z,
        as.numeric(d$w=="alpha")*as.numeric(d$x==1),as.numeric(d$w=="alpha")*as.numeric(d$x==3),as.numeric(d$w=="alpha")*d$z,
        as.numeric(d$w=="omega")*as.numeric(d$x==1),as.numeric(d$w=="omega")*as.numeric(d$x==3),as.numeric(d$w=="omega")*d$z,
        as.numeric(d$x==1)*d$z,as.numeric(d$x==3)*d$z,
        as.numeric(d$w=="alpha")*as.numeric(d$x==1)*d$z,as.numeric(d$w=="alpha")*as.numeric(d$x==3)*d$z,
        as.numeric(d$w=="omega")*as.numeric(d$x==1)*d$z,as.numeric(d$w=="omega")*as.numeric(d$x==3)*d$z)
    colnames(X)<-c("(Intercept)","c@1","w@0","w@2","x@0","x@2","z","w@0:x@0","w@0:x@2","w@0:z","w@2:x@0","w@2:x@2","w@2:z","x@0:z","x@2:z","w@0:x@0:z","w@0:x@2:z","w@2:x@0:z","w@2:x@2:z")
    testthat::expect_identical(eq$design,X)
    testthat::expect_equal(pl_fit(eq$design,eq$y)$coefficients,stats::lm.fit(X,d$m)$coefficients,tolerance=1e-12)
    testthat::expect_match(eq$equation,"x \\[1 vs 2\\]",fixed=FALSE)
    testthat::expect_match(eq$equation,'w ["omega" vs "base"]',fixed=TRUE)
    testthat::expect_setequal(names(eq$means),c("m","y","z"))
    testthat::expect_true(all(lengths(eq$means)==1L))
    testthat::expect_identical(eq$family,"linear")
    q<-f$settings;q$outcome_family<-"logistic";d$y<-rep(c(0,1),length.out=nrow(d));g<-f$graph;g$nodes[[3]]$measurement_scale<-"nominal"
    m<-validate_spec(g,q,names(d));ee<-compile_equations(m,prepare_sample(d,m))
    testthat::expect_identical(ee$m$family,"linear")
    testthat::expect_identical(ee$y$family,"logistic")
})

testthat::test_that("categorical contrasts and numeric derivatives use separate focal bases without cross contrasts",{
    f<-pl_cat_core_fixture();eq<-f$equations
    beta<-lapply(eq,function(e)setNames(rep(0,length(e$terms)),names(e$terms)))
    beta$m[c("x@0","x@2","w@0:x@0","w@2:x@0","w@0:x@2","w@2:x@2","x@0:z","x@2:z","w@0:x@0:z","w@2:x@2:z")]<-c(2,4,.5,1,1.5,-.5,.2,.4,.3,-.2)
    beta$y[c("m","x@0","x@2")]<-c(3,.25,-.75)
    for(level in c("alpha","base","omega"))for(z in c(-1,1))for(factor in c("x@0","x@2")) {
        first<-if(factor=="x@0")2+c(alpha=.5,base=0,omega=1)[level]+z*(.2+if(level=="alpha").3 else 0) else 4+c(alpha=1.5,base=0,omega=-.5)[level]+z*(.4-if(level=="omega").2 else 0)
        derivative<-if(factor=="x@0").2+(if(level=="alpha").3 else 0) else .4-(if(level=="omega").2 else 0)
        testthat::expect_equal(pl_route(c("p_x_m","p_m_y"),f$model,eq,beta,list(w=level,z=z),focal_factor=factor),unname(3*first))
        testthat::expect_equal(pl_route(c("p_x_m","p_m_y"),f$model,eq,beta,list(w=level,z=z),"z",factor),3*derivative)
    }
    rows<-evaluate_estimands(f$model,f$sample,eq,beta)
    testthat::expect_false(anyDuplicated(vapply(rows,`[[`,"","id"))>0)
    testthat::expect_setequal(unique(vapply(rows,`[[`,"","focal_factor")),c("x@0","x@2"))
    testthat::expect_true(all(vapply(rows,function(r)r$focal_contrast$reference==2,TRUE)))
    indices<-Filter(function(r)length(r$moderator_ids)>0,rows)
    testthat::expect_true(all(vapply(indices,function(r)identical(r$moderator_ids,"z"),TRUE)))
    testthat::expect_true(all(vapply(indices,function(r)r$kind=="conditional_index",TRUE)))
    byid<-setNames(rows,vapply(rows,`[[`,"","id"))
    for(r in Filter(function(r)!is.null(r$effect_a),rows))testthat::expect_identical(byid[[r$effect_a]]$focal_factor,byid[[r$effect_b]]$focal_factor)
    plan<-pl_compile_estimand_plan(f$model,f$sample,eq,rows)
    testthat::expect_identical(pl_evaluate_estimand_plan(plan,beta),vapply(rows,`[[`,0,"estimate"))
    beta$m["x@2"]<-7
    testthat::expect_identical(pl_evaluate_estimand_plan(plan,beta),vapply(evaluate_estimands(f$model,f$sample,eq,beta),`[[`,0,"estimate"))
})

testthat::test_that("categorical declarations reject ambiguous types and missing original levels",{
    f<-pl_cat_core_fixture()
    bad<-f$settings;bad$categorical[[1]]$levels<-list(1,"2",3)
    testthat::expect_error(validate_spec(f$graph,bad,names(f$data)),"INVALID_OPTIONS")
    bad<-f$settings;bad$categorical[[1]]$reference<-4
    testthat::expect_error(validate_spec(f$graph,bad,names(f$data)),"INVALID_OPTIONS")
    bad<-f$settings;bad$categorical[[1]]$levels<-list(1,1,3)
    testthat::expect_error(validate_spec(f$graph,bad,names(f$data)),"INVALID_OPTIONS")
    bad<-f$settings;bad$categorical[[1]]$node_id<-"m"
    testthat::expect_error(validate_spec(f$graph,bad,names(f$data)),"INVALID_OPTIONS")
    bad<-f$settings;bad$categorical<-bad$categorical[-2]
    testthat::expect_error(validate_spec(f$graph,bad,names(f$data)),"INVALID_OPTIONS")
    bad<-f$settings;bad$probes<-c(bad$probes,list(list(node_id="w",values=list(1))))
    testthat::expect_error(validate_spec(f$graph,bad,names(f$data)),"INVALID_OPTIONS")
    bad<-f$settings;bad$probes<-c(bad$probes,list(list(node_id="w",values=list("unknown"))))
    testthat::expect_error(validate_spec(f$graph,bad,names(f$data)),"INVALID_OPTIONS")
    d<-f$data;d$x<-as.character(d$x)
    testthat::expect_error(prepare_sample(d,f$model),"NUMERIC_TYPE_MISMATCH")
    d<-f$data;d$w<-rep(1:3,length.out=nrow(d))
    testthat::expect_error(prepare_sample(d,f$model),"INVALID_OPTIONS")
    d<-f$data;d$w<-factor(d$w,levels=c("omega","base","alpha"))
    testthat::expect_identical(prepare_sample(d,f$model)$basis_description,f$sample$basis_description)
    testthat::expect_error(prepare_sample(f$data[f$data$x!=3,],f$model),"INVALID_OPTIONS")
    g<-f$graph;g$nodes[[2]]$measurement_scale<-"nominal"
    testthat::expect_error(validate_spec(g,f$settings,names(f$data)),"INVALID_OPTIONS")
    g<-f$graph;g$nodes[[1]]$id<-"x@0"
    testthat::expect_error(validate_spec(g,f$settings,names(f$data)),"INVALID_MODEL")
    bad<-f$settings;bad$outcome_family<-"logistic"
    m<-validate_spec(f$graph,bad,names(f$data))
    testthat::expect_error(prepare_sample(f$data,m),"INVALID_OPTIONS")
})

testthat::test_that("bootstrap resamples retain the original categorical basis and count sparse failures",{
    f<-pl_cat_core_fixture(FALSE);eq<-fit_equations(f$equations,FALSE)
    idx<-matrix(rep(seq_len(f$sample$n),200),nrow=200,byrow=TRUE)
    sparse<-rep(which(f$data$x!=3),length.out=f$sample$n)
    idx[1:10,]<-rep(sparse,each=10)
    testthat::expect_error(pl_fit(eq$m$design[sparse,,drop=FALSE],eq$m$y[sparse]),"SINGULAR_DESIGN")
    result<-bootstrap_estimands(f$model,f$sample,eq,200L,419L,idx)
    testthat::expect_identical(result$accepted_draws,11:200)
    testthat::expect_identical(result$metadata$successful,190L)
    testthat::expect_equal(result$metadata$failure_reasons$SINGULAR_DESIGN,10)
    testthat::expect_identical(f$sample$basis$x,c("x@0","x@2"))
    idx[11,]<-sparse
    testthat::expect_error(bootstrap_estimands(f$model,f$sample,eq,200L,419L,idx),"BOOTSTRAP_FAILURE")
})
