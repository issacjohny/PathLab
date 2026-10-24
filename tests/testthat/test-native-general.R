pl_ng_dir<-function() {
    root<-getOption("pathlab.root")
    if(!is.null(root))file.path(root,"modules/pathlab/tests/fixtures/native-general") else testthat::test_path("..","fixtures","native-general")
}
pl_ng_read<-function(name,suffix)jsonlite::fromJSON(file.path(pl_ng_dir(),paste0(name,"-",suffix,".json")),simplifyVector=FALSE)
pl_ng_options<-function(q) {
    o<-q$native_options
    for(name in c("generalMediators","generalModerators","generalCovariates"))o[[name]]<-unlist(o[[name]],use.names=FALSE)
    o
}
pl_ng_conditions<-function(c)if(length(c))paste(sort(paste(names(c),sprintf("%.12g",unlist(c)),sep="=")),collapse="|") else ""
pl_ng_route<-function(path)paste(unlist(path),collapse=">")
pl_ng_key<-function(r) {
    indexed<-length(r$moderator_ids)>0
    kind<-if(indexed)r$effect_kind else r$kind
    paste(kind,if(kind=="specific_indirect")pl_ng_route(r$path_ids) else "",paste(sort(unlist(r$moderator_ids)),collapse=","),pl_ng_conditions(r$condition),sep="|")
}
pl_ng_graph_assert<-function(g,expected) {
    nodes<-function(s)vapply(s$nodes,function(n)paste(n$id,n$column,n$role,n$measurement_scale,sep="|"),"")
    edges<-function(s)vapply(s$edges,function(e)paste(e$id,e$source,e$target,sep="|"),"")
    mods<-function(s)vapply(s$moderations,function(m)paste(m$id,m$path_id,paste(sort(unlist(m$moderator_ids)),collapse=","),sep="|"),"")
    testthat::expect_setequal(nodes(g),nodes(expected))
    testthat::expect_setequal(edges(g),edges(expected))
    testthat::expect_setequal(mods(g),mods(expected))
}
pl_ng_resolve<-function(o,columns) {
    g<-native_spec(o);settings<-native_settings(o,g)
    validate_spec(g,settings,columns)
}
pl_ng_data<-function(name)read.csv(file.path(pl_ng_dir(),paste0(name,".csv")),check.names=FALSE)
pl_ng_coefficients<-function(ref,bootstrap=FALSE)lapply(if(bootstrap)ref$bootstrap_coefficients else ref$equations$false,function(v)if(bootstrap)do.call(rbind,lapply(v,unlist)) else unlist(v$beta))
pl_ng_public_effects<-function(tables) {
    frames<-lapply(tables$effectPages$items,function(page)page$asDF)
    if(!length(frames))return(data.frame())
    out<-do.call(rbind,frames);rownames(out)<-NULL;out
}
pl_ng_polynomial<-function(beta,condition,derivative,joint) {
    B<-function(target,index)if(is.matrix(beta[[target]]))beta[[target]][,index] else beta[[target]][index]
    value<-function(id)if(is.null(condition[[id]]))0 else condition[[id]]
    w<-value("w");z<-value("z");v<-value("v")
    a<-B("m1",2);u<-B("m1",if(joint)5 else 4)
    A<-a+u*w+if(joint)B("m1",6)*z+B("m1",8)*w*z else 0
    E<-B("m2",3)+if(joint)0 else B("m2",5)*z
    H<-B("m3",4)+if(joint)0 else B("m3",6)*v
    b<-B("m2",2);c<-B("m3",2);d<-B("y",2);f<-B("m3",3)
    g<-B("y",3);i<-B("y",4);j<-B("y",5);zero<-0*a
    if(!length(derivative))return(list(routes=list(A*g,b*i,c*j,A*E*i,A*f*j,b*H*j,A*E*H*j),direct=d))
    der<-sort(unlist(derivative))
    if(joint) {
        partial<-switch(paste(der,collapse=","),w=u+B("m1",8)*z,z=B("m1",6)+B("m1",8)*w,"w,z"=B("m1",8))
        return(list(routes=list(partial*g,zero,zero,partial*E*i,partial*f*j,zero,partial*E*H*j),direct=zero))
    }
    AA<-if("w" %in% der)u else A;EE<-if("z" %in% der)B("m2",5) else E;HH<-if("v" %in% der)B("m3",6) else H
    list(routes=list(if(all(der %in% "w"))AA*g else zero,zero,zero,if(all(der %in% c("w","z")))AA*EE*i else zero,if(all(der %in% "w"))AA*f*j else zero,if(all(der %in% "v"))b*HH*j else zero,AA*EE*HH*j),direct=zero)
}
pl_ng_expected<-function(row,beta,joint) {
    p<-pl_ng_polynomial(beta,row$condition,row$moderator_ids,joint)
    routes<-c("p_x_m1>p_m1_y","p_x_m2>p_m2_y","p_x_m3>p_m3_y","p_x_m1>p_m1_m2>p_m2_y","p_x_m1>p_m1_m3>p_m3_y","p_x_m2>p_m2_m3>p_m3_y","p_x_m1>p_m1_m2>p_m2_m3>p_m3_y")
    kind<-if(length(row$moderator_ids))row$effect_kind else row$kind
    switch(kind,specific_indirect=p$routes[[match(pl_ng_route(row$path_ids),routes)]],total_indirect=Reduce("+",p$routes),direct=p$direct,total=Reduce("+",p$routes)+p$direct)
}

testthat::test_that("general control rows independently preserve three mediators and joint hierarchy",{
    for(name in c("joint3_two","separate3_three")) {
        q<-pl_ng_read(name,"request");o<-pl_ng_options(q);g<-native_spec(o)
        pl_ng_graph_assert(g,q$graph)
        # JSON fixture arrays and native numeric vectors encode the same
        # public settings; normalize only that representation difference.
        normalize<-function(x)jsonlite::fromJSON(jsonlite::toJSON(x,auto_unbox=TRUE),simplifyVector=FALSE)
        testthat::expect_equal(normalize(native_settings(o,g)),normalize(q$settings))
        testthat::expect_length(Filter(function(n)n$role=="mediator",g$nodes),3)
        model<-pl_ng_resolve(o,names(pl_ng_data(name)))
        testthat::expect_equal(sum(vapply(model$routes,function(r)length(r$path_ids)>1,TRUE)),7)
    }
    q<-pl_ng_read("joint3_two","request");o<-pl_ng_options(q)
    o$generalMediators<-c("m3","m1","m2")
    g<-native_spec(o);by<-setNames(g$nodes,pl_ids(g$nodes))
    testthat::expect_identical(vapply(by[c("m1","m2","m3")],function(n)n$column,""),c(m1="m3",m2="m1",m3="m2"))
    testthat::expect_true("p_cov1_y" %in% pl_ids(g$edges))
    testthat::expect_false(any(c("p_cov1_m1","p_cov1_m2","p_cov1_m3") %in% pl_ids(g$edges)))
})

testthat::test_that("general ten-mediator three-moderator twenty-covariate limits preserve every slot",{
    o<-list(modelMode="general",x="x",y="y",generalMediators=paste0("med",1:10),generalModerators=c("wcol","zcol","vcol"),generalCovariates=paste0("c",1:20),generalProbesW="-2,-1",generalProbesZ="0,1",generalProbesV="2,3")
    o$generalPaths<-c(list(list(source="X",target="M1")),lapply(1:9,function(k)list(source=paste0("M",k),target=paste0("M",k+1))),list(list(source="M10",target="Y"),list(source="X",target="Y"),list(source="W",target="M1"),list(source="Z",target="M5"),list(source="V",target="M10")),lapply(1:20,function(k)list(source=paste0("C",k),target="Y")))
    o$generalModerations<-list(list(source="X",target="M1",mod1="W",mod2="none"),list(source="M4",target="M5",mod1="Z",mod2="none"),list(source="M9",target="M10",mod1="V",mod2="none"))
    g<-native_spec(o);columns<-c("x","y",o$generalMediators,o$generalModerators,o$generalCovariates)
    model<-pl_ng_resolve(o,columns);by<-setNames(g$nodes,pl_ids(g$nodes))
    testthat::expect_length(g$nodes,35)
    testthat::expect_length(g$edges,35)
    testthat::expect_length(model$routes,2)
    testthat::expect_identical(by$m10$column,"med10")
    testthat::expect_identical(by$v$column,"vcol")
    testthat::expect_identical(by$cov20$column,"c20")
    testthat::expect_identical(tail(model$routes[[which(vapply(model$routes,function(r)length(r$path_ids)>1,TRUE))]]$node_ids,2),c("m10","y"))
    optionclass<-if(exists("pathsOptions",inherits=TRUE))get("pathsOptions",inherits=TRUE) else getFromNamespace("pathsOptions","pathlab")
    native_options<-do.call(optionclass$new,o)
    testthat::expect_setequal(unique(unlist(native_options$varsRequired,use.names=FALSE)),columns)
    testthat::expect_identical(native_options$generalMediators,paste0("med",1:10))
    testthat::expect_identical(native_options$generalModerators,c("wcol","zcol","vcol"))
    testthat::expect_identical(native_options$generalCovariates,paste0("c",1:20))
    testthat::expect_identical(c(native_options$generalProbesW,native_options$generalProbesZ,native_options$generalProbesV),c("-2,-1","0,1","2,3"))
    probes<-native_settings(o,g)$probes
    testthat::expect_equal(setNames(lapply(probes,function(p)p$values),vapply(probes,function(p)p$node_id,"")),list(w=c(-2,-1),z=c(0,1),v=c(2,3)))
    for(name in c("generalMediators","generalModerators","generalCovariates")) {
        bad<-o;bad[[name]]<-c(o[[name]],"extra")
        testthat::expect_error(native_spec(bad),"RESOURCE_LIMIT")
    }
})

testthat::test_that("general hierarchy self-moderation duplicate and missing-role requests are rejected",{
    q<-pl_ng_read("joint3_two","request");original<-pl_ng_options(q);columns<-names(pl_ng_data("joint3_two"))
    for(k in 1:3) {
        bad<-original;bad$generalModerations<-bad$generalModerations[-k]
        testthat::expect_error(pl_ng_resolve(bad,columns),"INVALID_MODEL")
    }
    for(slot in c("W","Z")) {
        bad<-original;bad$generalPaths<-Filter(function(r)!(r$source==slot && r$target=="M1"),bad$generalPaths)
        testthat::expect_error(pl_ng_resolve(bad,columns),"INCOMPLETE_MODEL|INVALID_MODEL")
    }
    bad<-original;bad$generalPaths<-c(bad$generalPaths,bad$generalPaths[1])
    testthat::expect_error(native_spec(bad),"duplicate")
    bad<-original;bad$generalModerations<-c(bad$generalModerations,list(list(source="Z",target="M1",mod1="W",mod2="none")))
    testthat::expect_error(pl_ng_resolve(bad,columns),"INVALID_MODEL")
    bad<-original;bad$generalModerations<-c(bad$generalModerations,list(list(source="X",target="M1",mod1="Z",mod2="W")))
    testthat::expect_error(pl_ng_resolve(bad,columns),"INVALID_MODEL")
    bad<-original;bad$generalModerations[[1]]<-list(source="W",target="M1",mod1="W",mod2="none")
    testthat::expect_error(native_spec(bad),"cannot moderate itself")
    bad<-original;bad$generalMediators<-bad$generalMediators[1:2]
    testthat::expect_error(native_spec(bad),"INCOMPLETE_MODEL")
    bad<-original;bad$generalMediators[3]<-"x"
    testthat::expect_error(native_spec(bad),"only one role")
    bad<-original;bad$generalPaths<-c(bad$generalPaths,list(list(source="M2",target="M1")))
    testthat::expect_error(pl_ng_resolve(bad,columns),"INVALID_MODEL")
    # A single opposite orientation of the same W:Z term is equivalent.
    equivalent<-original;equivalent$generalModerations[[3]]<-list(source="Z",target="M1",mod1="W",mod2="none")
    d<-pl_ng_data("joint3_two");a<-pl_ng_resolve(original,columns);b<-pl_ng_resolve(equivalent,columns);s<-prepare_sample(d,a)
    testthat::expect_equal(lapply(compile_equations(a,s),function(e)e$design),lapply(compile_equations(b,s),function(e)e$design))
})

testthat::test_that("general optional lists and explicit probes have bounded predictable semantics",{
    q<-pl_ng_read("separate3_three","request");o<-pl_ng_options(q);d<-pl_ng_data("separate3_three")
    for(value in c("-.5,","-.5,,.7","-.5, ,.7","-.5,-.5","NaN","Inf","1e999","word")) {
        bad<-o;bad$generalProbesW<-value
        testthat::expect_error(native_settings(bad,native_spec(bad)),"INVALID_OPTIONS")
    }
    o$generalProbesW<-"";o$generalProbesZ<-"";o$generalProbesV<-""
    testthat::expect_error(prepare_sample(d,pl_ng_resolve(o,names(d))),"RESOURCE_LIMIT")
    o$generalProbesW<-"-.5,.7"
    testthat::expect_length(prepare_sample(d,pl_ng_resolve(o,names(d)))$conditions,18)
    empty<-list(modelMode="general",x="x",y="y",generalMediators=character(),generalModerators="w",generalCovariates=character(),generalPaths=list(list(source="X",target="Y"),list(source="W",target="Y")),generalModerations=list(list(source="X",target="Y",mod1="W",mod2="none")))
    testthat::expect_length(pl_ng_resolve(empty,names(d))$routes,1)
    empty$generalModerators<-character();empty$generalModerations<-list();empty$generalPaths<-empty$generalPaths[1]
    testthat::expect_error(pl_ng_resolve(empty,names(d)),"INVALID_MODEL")
    empty$generalMediators<-"m1";empty$generalPaths<-list(list(source="X",target="M1"),list(source="M1",target="Y"))
    testthat::expect_length(pl_ng_resolve(empty,names(d))$routes,1)
})

pl_ng_cache<-new.env(parent=emptyenv())
for(case in c("joint3_two","separate3_three"))local({
    name<-case;joint<-name=="joint3_two"
    testthat::test_that(paste(name,"native mapping matches saved-index Python and independent four-equation references"),{
        q<-pl_ng_read(name,"request");o<-pl_ng_options(q);d<-pl_ng_data(name);g<-native_spec(o);settings<-native_settings(o,g)
        independent<-pl_ng_read(name,"independent");pyall<-pl_ng_read(name,"python")
        indices<-as.matrix(read.csv(file.path(pl_ng_dir(),paste0(name,"-indices.csv")),header=FALSE))
        testthat::expect_equal(unname(indices),unname(do.call(rbind,lapply(independent$resample_indices_1based,unlist))))
        r<-pathlab_fit(d,g,settings,draws=200,seed=q$seed,robust=FALSE,indices=indices)
        pl_ng_cache[[name]]<-r
        testthat::expect_equal(r$sample$n,120)
        testthat::expect_equal(r$bootstrap$successful,200)
        testthat::expect_equal(r$accepted_draws,1:200)
        testthat::expect_length(r$estimands,if(joint)409 else 1564)
        model<-validate_spec(g,settings,names(d));sample<-prepare_sample(d,model);compiled<-compile_equations(model,sample)
        canonical<-function(term)if(term=="(Intercept)")term else paste(sort(ifelse(strsplit(term,":",fixed=TRUE)[[1]]=="c1","cov1",strsplit(term,":",fixed=TRUE)[[1]])),collapse=":")
        for(robust in c(FALSE,TRUE)) {
            fitted<-fit_equations(compiled,robust)
            for(target in names(fitted)) {
                eq<-fitted[[target]];terms<-unlist(independent$terms[[target]]);order<-match(names(eq$terms),terms)
                ref<-independent$equations[[tolower(as.character(robust))]][[target]]
                beta<-unlist(ref$beta);cov<-do.call(rbind,lapply(ref$covariance,unlist))
                testthat::expect_false(anyNA(order))
                testthat::expect_equal(unname(eq$fit$coefficients),unname(beta[order]),tolerance=1e-10)
                testthat::expect_equal(unname(eq$fit$covariance),unname(cov[order,order]),tolerance=1e-10)
                for(stat in c("se","p","lower","upper"))testthat::expect_equal(unname(eq$fit[[stat]]),unname(unlist(ref[[stat]])[order]),tolerance=1e-10)
                raw<-stats::lm(as.formula(paste(target,"~",q$reference_formulas[[target]])),d)
                X<-stats::model.matrix(raw);ord<-match(names(eq$terms),vapply(colnames(X),canonical,""))
                testthat::expect_equal(as.vector(eq$design),as.vector(X[,ord,drop=FALSE]),tolerance=1e-12)
                testthat::expect_equal(unname(eq$fit$coefficients),unname(stats::coef(raw)[ord]),tolerance=1e-10)
            }
            diagnostics<-pl_slopes(model,sample,fitted,.05);py<-pyall[[tolower(as.character(robust))]]
            testthat::expect_length(diagnostics$slopes,length(py$slopes))
            for(p in py$slopes) {
                rr<-Filter(function(x)x$path_id==p$path_id && pl_ng_conditions(x$condition)==pl_ng_conditions(p$condition),diagnostics$slopes)
                testthat::expect_length(rr,1)
                for(stat in c("estimate","se","lower","upper","p"))testthat::expect_equal(rr[[1]][[stat]],p[[stat]],tolerance=1e-8)
            }
            testthat::expect_length(diagnostics$johnson_neyman,length(py$johnson_neyman))
            for(p in py$johnson_neyman) {
                rr<-Filter(function(x)x$path_id==p$path_id && x$moderator_id==p$moderator_id && pl_ng_conditions(x$other_conditions)==pl_ng_conditions(p$other_conditions),diagnostics$johnson_neyman)
                testthat::expect_length(rr,1)
                testthat::expect_equal(rr[[1]]$boundaries,as.numeric(unlist(p$boundaries)),tolerance=1e-8)
            }
        }
        beta<-pl_ng_coefficients(independent);boot<-pl_ng_coefficients(independent,TRUE)
        expected<-list();published<-setNames(r$estimands,vapply(r$estimands,function(x)x$id,""))
        defaults<-setNames(lapply(settings$probes,function(p)unlist(p$values)[1]),vapply(settings$probes,function(p)p$node_id,""))
        refkey<-function(x)paste(x$kind,if(x$kind=="specific_indirect")pl_ng_route(x$path_ids) else "",paste(sort(unlist(x$moderator_ids)),collapse=","),pl_ng_conditions(x$condition),sep="|")
        references<-setNames(independent$estimands,vapply(independent$estimands,refkey,""))
        for(row in Filter(function(x)is.null(x$effect_a),r$estimands)) {
            expected[[row$id]]<-list(value=pl_ng_expected(row,beta,joint),draws=pl_ng_expected(row,boot,joint))
            keyrow<-row;if(!length(keyrow$condition))keyrow$condition<-defaults
            ref<-references[[pl_ng_key(keyrow)]]
            testthat::expect_false(is.null(ref))
            for(stat in c("estimate","bootstrap_se","lower","upper"))testthat::expect_equal(row[[stat]],ref[[stat]],tolerance=1e-8)
        }
        for(row in Filter(function(x)!is.null(x$effect_a),r$estimands))expected[[row$id]]<-list(value=expected[[row$effect_a]]$value-expected[[row$effect_b]]$value,draws=expected[[row$effect_a]]$draws-expected[[row$effect_b]]$draws)
        for(k in seq_along(r$estimands)) {
            row<-r$estimands[[k]];ref<-expected[[row$id]]
            testthat::expect_equal(row$estimate,unname(ref$value),tolerance=1e-8)
            testthat::expect_equal(r$bootstrap_distribution[,k],unname(ref$draws),tolerance=1e-8)
            testthat::expect_equal(row$bootstrap_se,stats::sd(ref$draws),tolerance=1e-8)
            testthat::expect_equal(c(row$lower,row$upper),unname(stats::quantile(ref$draws,c(.025,.975),type=7)),tolerance=1e-8)
        }
        # Actual preserved-Python output rows are required, including every
        # paired contrast; source IDs are matched by mathematical semantics.
        originals<-Filter(function(x)is.null(x$effect_a),r$estimands)
        lookup<-setNames(originals,vapply(originals,pl_ng_key,""))
        py<-pyall$false
        for(p in c(py$effects,py$indices)) {
            actual<-lookup[[pl_ng_key(p)]]
            testthat::expect_false(is.null(actual))
            for(stat in c("estimate","bootstrap_se","lower","upper"))testthat::expect_equal(actual[[stat]],p[[stat]],tolerance=1e-8)
        }
        contrast_key<-function(a,b)paste(a,b,sep="|")
        contrast_rows<-Filter(function(x)!is.null(x$effect_a),r$estimands)
        contrasts<-setNames(contrast_rows,vapply(contrast_rows,function(x)contrast_key(x$effect_a,x$effect_b),""))
        for(group in c("effect_contrasts","index_contrasts")) {
            indexed<-group=="index_contrasts";sources<-if(indexed)py$indices else py$effects
            byid<-setNames(sources,vapply(sources,function(x)x$id,""))
            for(p in py[[group]]) {
                a<-lookup[[pl_ng_key(byid[[if(indexed)p$index_a else p$effect_a]])]]
                b<-lookup[[pl_ng_key(byid[[if(indexed)p$index_b else p$effect_b]])]]
                actual<-contrasts[[contrast_key(a$id,b$id)]];sign<-1
                if(is.null(actual)){actual<-contrasts[[contrast_key(b$id,a$id)]];sign<--1}
                testthat::expect_false(is.null(actual))
                testthat::expect_equal(actual$estimate,sign*p$estimate,tolerance=1e-8)
                testthat::expect_equal(actual$bootstrap_se,p$bootstrap_se,tolerance=1e-8)
                testthat::expect_equal(c(actual$lower,actual$upper),if(sign==1)c(p$lower,p$upper) else -c(p$upper,p$lower),tolerance=1e-8)
            }
        }
        testthat::expect_false(anyDuplicated(vapply(r$estimands,function(x)x$id,""))>0)
    })
    testthat::test_that(paste(name,"actual generated public paths interface retains all variables and result tables"),{
        testthat::skip_if_not_installed("jmvcore")
        q<-pl_ng_read(name,"request");o<-pl_ng_options(q);d<-pl_ng_data(name)
        public<-if(exists("paths",inherits=TRUE))get("paths",inherits=TRUE) else getFromNamespace("paths","pathlab")
        optionclass<-if(exists("pathsOptions",inherits=TRUE))get("pathsOptions",inherits=TRUE) else getFromNamespace("pathsOptions","pathlab")
        options<-do.call(optionclass$new,o)
        testthat::expect_setequal(unique(unlist(options$varsRequired,use.names=FALSE)),names(d))
        testthat::expect_identical(options$generalMediators,c("m1","m2","m3"))
        testthat::expect_identical(options$generalModerations,o$generalModerations)
        tables<-do.call(public,c(list(data=d),o))
        result<-jsonlite::fromJSON(tables$reproduction$content,simplifyVector=FALSE)
        pl_ng_graph_assert(result$graph,q$graph)
        testthat::expect_equal(result$sample$n,120)
        testthat::expect_equal(nrow(tables$equations$asDF),4)
        testthat::expect_equal(nrow(tables$estimates$asDF),21)
        pages<-tables$effectPages$items;effect_rows<-pl_ng_public_effects(tables)
        testthat::expect_length(pages,if(joint)9 else 32)
        testthat::expect_equal(as.integer(unlist(tables$effectPages$itemKeys)),seq_along(pages))
        testthat::expect_true(tables$effectPages$visible)
        testthat::expect_true(all(vapply(pages,function(page)page$visible,TRUE)))
        testthat::expect_true(all(vapply(pages,function(page)inherits(page,"Table"),TRUE)))
        counts<-vapply(pages,function(page)nrow(page$asDF),0L)
        testthat::expect_equal(counts,c(rep(50L,length(pages)-1L),if(joint)9L else 14L))
        testthat::expect_equal(nrow(effect_rows),if(joint)409 else 1564)
        keys<-unlist(lapply(pages,function(page)page$rowKeys),use.names=FALSE)
        testthat::expect_identical(keys,vapply(result$estimands,function(row)row$id,""))
        testthat::expect_equal(anyDuplicated(keys),0L)
        testthat::expect_equal(nrow(tables$effects$asDF),0L)
        testthat::expect_false(tables$effects$visible)
        testthat::expect_equal(nrow(tables$simpleSlopes$asDF),if(joint)6 else 24)
        testthat::expect_equal(nrow(tables$jn$asDF),if(joint)5 else 12)
        # Check the public result's complete display mapping, including the
        # paired-bootstrap values from this native call's own saved seed.
        display_rows<-lapply(result$estimands,function(row){row$path_ids<-unlist(row$path_ids,use.names=FALSE);row})
        lookup<-setNames(display_rows,vapply(display_rows,function(row)row$id,""))
        expected_cells<-do.call(rbind,lapply(display_rows,function(row)data.frame(
            kind=pl_association_label(row),route=pl_estimand_label(result$graph,row,lookup=lookup),
            condition=paste(pl_condition_label(result$graph,row$condition),if(length(row$condition_b))paste("minus",pl_condition_label(result$graph,row$condition_b))),
            estimate=row$estimate,se=row$bootstrap_se,lower=row$lower,upper=row$upper,stringsAsFactors=FALSE)))
        rownames(expected_cells)<-NULL
        testthat::expect_equal(effect_rows,expected_cells,tolerance=1e-10)
        # Independent point polynomials let this public-interface case run
        # alone, without relying on the earlier saved-index fit's cache.
        coefficients<-pl_ng_coefficients(pl_ng_read(name,"independent"));points<-list()
        for(row in result$estimands)points[[row$id]]<-if(is.null(row$effect_a))pl_ng_expected(row,coefficients,joint) else points[[row$effect_a]]-points[[row$effect_b]]
        expected_points<-unname(unlist(points))
        testthat::expect_equal(effect_rows$estimate,expected_points,tolerance=1e-8)
        testthat::expect_equal(vapply(result$estimands,function(x)x$estimate,0.0),expected_points,tolerance=1e-8)
        if(joint)testthat::expect_true(any(grepl("at means:",tables$simpleSlopes$asDF$condition,fixed=TRUE)))
    })
})
