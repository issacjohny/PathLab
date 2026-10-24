# Reproduce synthetic PathLab Monte Carlo cells using an installed module.
# Usage: Rscript simulation_study.R serial_alt 100 1 1000 2000 output-folder
# Install a matching PathLab package first. No Python or external service needed.
args<-commandArgs(trailingOnly=TRUE)
library(pathlab)
stopifnot(as.character(packageVersion('pathlab'))=='0.4.6')
provenance<-system.file('provenance/build-source.json',package='pathlab')
source<-if(nzchar(provenance))jsonlite::fromJSON(provenance) else list(source_revision='unrecorded')
RNGkind('Mersenne-Twister','Inversion','Rejection')

scenarios <- c('serial_alt','serial_null','serial_double_null','repeated_w',
    'joint_wz','categorical','logistic_alt','logistic_null','heteroscedastic','separation_stress')

configuration <- function(name) {
    pairs <- list(c('x','m1'),c('m1','m2'),c('m2','y'),c('x','y'),c('w','m1'))
    mods <- list(list('x','m1',c('w')))
    if(name=='repeated_w') {
        pairs <- c(pairs,list(c('w','m2'),c('w','y')))
        mods <- c(mods,list(list('m1','m2','w'),list('m2','y','w')))
    }
    if(name=='joint_wz') {
        pairs <- c(pairs,list(c('z','m1')))
        mods <- c(mods,list(list('x','m1','z'),list('w','m1','z'),list('x','m1',c('w','z'))))
    }
    ids <- sort(unique(unlist(pairs)))
    role <- c(x='independent',m1='mediator',m2='mediator',y='dependent',w='moderator',z='moderator')
    graph <- list(schema_version=1L,nodes=lapply(ids,function(i)list(id=i,column=i,role=unname(role[i]),measurement_scale=if(name=='categorical' && i %in% c('x','w'))'nominal' else 'interval')),
        edges=lapply(pairs,function(p)list(id=paste('p',p[1],p[2],sep='_'),source=p[1],target=p[2])),
        moderations=lapply(seq_along(mods),function(i)list(id=paste0('mod',i),path_id=paste('p',mods[[i]][1],mods[[i]][2],sep='_'),moderator_ids=as.list(mods[[i]][[3]]))))
    # Explicit scalars, not lists, for source/target/path_id.
    for(i in seq_along(mods))graph$moderations[[i]]$path_id <- paste('p',mods[[i]][[1]],mods[[i]][[2]],sep='_')
    probes <- lapply(intersect(ids,c('w','z')),function(i)list(node_id=i,values=as.list(c(-1,0,1))))
    settings <- list(schema_version=3L,outcome_family=if(grepl('logistic|separation',name))'logistic' else 'linear',categorical=list(),probes=probes,
        automatic_probes=list(probabilities=as.list(c(.16,.5,.84)),quantile_method='linear',sample='fixed_complete_case_sample',resolve_before_bootstrap=TRUE),bootstrap_interval_quantile_method='linear')
    if(name=='categorical') {
        settings$categorical <- list(list(node_id='x',levels=as.list(c('A','B','C')),reference='A'),list(node_id='w',levels=as.list(c('low','high')),reference='low'))
        settings$probes <- list()
    }
    list(graph=graph,settings=settings,robust=name=='heteroscedastic')
}

generate <- function(name,n,seed) {
    set.seed(seed)
    x<-rnorm(n);w<-rnorm(n);z<-rnorm(n)
    a<-.5;u<-.3;b<-.6;cc<-.5
    if(name %in% c('serial_null','serial_double_null','logistic_null')) {a<-0;u<-0}
    if(name=='serial_double_null')b<-0
    if(name=='heteroscedastic')u<-0
    A<-a*x+.2*w+u*x*w
    if(name=='repeated_w')A<-.5*x+.2*w+.2*x*w
    if(name=='joint_wz')A<-.5*x+.2*w+.1*z+.2*x*w+.15*x*z+.12*w*z+.12*x*w*z
    if(name=='categorical') {
        x<-sample(c('A','B','C'),n,TRUE,c(.55,.30,.15));w<-sample(c('low','high'),n,TRUE,c(.65,.35))
        A<-.5*(x=='B')+.2*(w=='high')+.3*(x=='B')*(w=='high')
    }
    noise<-rnorm(n)
    if(name=='heteroscedastic')noise<-noise*sqrt(.3+.7*x^2)
    m1<-.2+A+noise
    m2<-.1+b*m1+if(name=='repeated_w') .1*w+.15*m1*w+rnorm(n) else rnorm(n)
    eta<--.3+cc*m2 # direct X -> Y is explicitly present but population-null.
    if(name=='repeated_w')eta<-eta+.1*w+.1*m2*w
    if(grepl('logistic',name))y<-rbinom(n,1,plogis(eta)) else y<-eta+rnorm(n)
    if(name=='separation_stress')y<-as.numeric(m2>0)
    data.frame(x=x,w=w,z=z,m1=m1,m2=m2,y=y,stringsAsFactors=FALSE)
}

# Independently declared population indirect polynomials in W,Z. Columns:
# coefficient, W exponent, Z exponent. No estimated coefficients/compiler use.
polynomial <- function(name,level=NULL) {
    switch(name,
      serial_alt=rbind(c(.15,0,0),c(.09,1,0)),
      serial_null=matrix(c(0,0,0),1),serial_double_null=matrix(c(0,0,0),1),
      repeated_w=rbind(c(.15,0,0),c(.1275,1,0),c(.0345,2,0),c(.003,3,0)),
      joint_wz=rbind(c(.15,0,0),c(.06,1,0),c(.045,0,1),c(.036,1,1)),
      categorical=if(identical(level,'B'))rbind(c(.15,0,0),c(.09,1,0)) else matrix(c(0,0,0),1),
      logistic_alt=rbind(c(.15,0,0),c(.09,1,0)),logistic_null=matrix(c(0,0,0),1),
      heteroscedastic=matrix(c(.15,0,0),1),separation_stress=matrix(c(NA,0,0),1))
}
effect_truth <- function(row,name,rows) {
    if(!is.null(row$effect_a))return(effect_truth(rows[[row$effect_a]],name,rows)-effect_truth(rows[[row$effect_b]],name,rows))
    if(row$kind=='direct')return(0)
    level<-if(!is.null(row$focal_contrast))row$focal_contrast$level else NULL
    p<-polynomial(name,level)
    cond<-row$condition
    w<-if(is.null(cond$w))0 else if(is.character(cond$w))as.numeric(cond$w=='high') else cond$w
    z<-if(is.null(cond$z))0 else cond$z
    derivative<-unlist(row$moderator_ids)
    for(d in derivative) {col<-if(d=='w')2 else 3;p[,1]<-p[,1]*p[,col];p[,col]<-pmax(0,p[,col]-1)}
    sum(p[,1]*w^p[,2]*z^p[,3])
}

# Explicit population coefficients by unordered factor identity. Independent of
# the production column order; categorical factors are mapped using descriptors.
coefficient_truth <- function(name,target,terms,description) {
    a<-.5;u<-.3;b<-.6
    if(name %in% c('serial_null','serial_double_null','logistic_null')){a<-0;u<-0}
    if(name=='serial_double_null')b<-0
    if(name=='heteroscedastic')u<-0
    if(name=='repeated_w')u<-.2
    key<-function(term)paste(sort(vapply(term,function(f) {
        desc<-description[[f]]
        if(!is.null(desc$level))paste0(desc$node_id,'=',desc$level) else f
    },'')),collapse=':')
    vapply(terms,function(t) {
        k<-key(t)
        if(!length(t))return(switch(target,m1=.2,m2=.1,y=-.3))
        map<-switch(target,m1=c(x=a,w=.2,'w:x'=u),m2=c(m1=b),y=c(m2=.5,x=0))
        if(name=='repeated_w') {
            if(target=='m2')map<-c(map,w=.1,'m1:w'=.15)
            if(target=='y')map<-c(map,w=.1,'m2:w'=.1)
        }
        if(name=='joint_wz' && target=='m1')map<-c(x=.5,w=.2,z=.1,'w:x'=.2,'x:z'=.15,'w:z'=.12,'w:x:z'=.12)
        if(name=='categorical') {
            if(target=='m1')map<-c('x=B'=.5,'x=C'=0,'w=high'=.2,'w=high:x=B'=.3,'w=high:x=C'=0)
            if(target=='y')map<-c(m2=.5,'x=B'=0,'x=C'=0)
        }
        if(k %in% names(map))unname(map[k]) else stop(paste('Undeclared population coefficient',name,target,k))
    },0.)
}

independent_reference <- function(d,name,equations,robust) {
    one<-rep(1,nrow(d));x<-d$x;w<-d$w;z<-d$z
    first<-if(name=='categorical')NULL else cbind(one,w,x,'w:x'=w*x)
    if(name=='joint_wz')first<-cbind(one,w,x,z,'w:x'=w*x,'w:z'=w*z,'x:z'=x*z,'w:x:z'=w*x*z)
    second<-cbind(one,m1=d$m1);last<-cbind(one,m2=d$m2,x=x)
    if(name=='repeated_w') {
        second<-cbind(second,w=w,'m1:w'=d$m1*w)
        last<-cbind(last,w=w,'m2:w'=d$m2*w)
    }
    if(name=='categorical') {
        xb<-as.numeric(x=='B');xc<-as.numeric(x=='C');hi<-as.numeric(w=='high')
        first<-cbind(one,'x=B'=xb,'x=C'=xc,'w=high'=hi,'w=high:x=B'=hi*xb,'w=high:x=C'=hi*xc)
        last<-cbind(one,m2=d$m2,'x=B'=xb,'x=C'=xc)
    }
    designs<-list(m1=first,m2=second,y=last)
    deltas<-c(coefficient=0,covariance=0)
    for(target in names(designs)) {
        X<-designs[[target]];colnames(X)[1]<-'(Intercept)';y<-d[[target]]
        if(target=='y' && grepl('logistic',name)) {
            f<-stats::glm.fit(X,y,family=stats::binomial(),control=stats::glm.control(epsilon=1e-12,maxit=100))
            beta<-f$coefficients;weights<-f$fitted.values*(1-f$fitted.values)
            covariance<-solve(crossprod(X,X*weights))
        } else {
            # Deliberately different algebra from production's lm.fit/QR.
            bread<-solve(crossprod(X));beta<-as.numeric(bread%*%crossprod(X,y));names(beta)<-colnames(X)
            residual<-y-as.numeric(X%*%beta)
            covariance<-bread*sum(residual^2)/(nrow(X)-ncol(X))
            if(robust) {
                leverage<-rowSums((X%*%bread)*X)
                covariance<-bread%*%crossprod(X,X*(residual/(1-leverage))^2)%*%bread
            }
        }
        e<-equations[[target]]
        labels<-vapply(e$terms,function(t)if(!length(t))'(Intercept)' else paste(sort(vapply(t,function(f) {
            desc<-e$basis_description[[f]]
            if(!is.null(desc$level))paste0(desc$node_id,'=',desc$level) else f
        },'')),collapse=':'),'')
        positions<-match(labels,colnames(X));stopifnot(!anyNA(positions))
        deltas<-pmax(deltas,c(max(abs(beta[positions]-e$fit$coefficients)),max(abs(covariance[positions,positions]-e$fit$covariance))))
    }
    stopifnot(all(deltas<1e-7))
    deltas
}

run_rep <- function(name,n,replicate,draws,save_dir=NULL) {
    si<-match(name,scenarios)
    generation_seed<-20261023L+si*100000L+n*100L+replicate
    bootstrap_seed<-generation_seed+10000000L
    d<-generate(name,n,generation_seed);q<-configuration(name)
    idx<-NULL
    if(!is.null(save_dir)) {
        dir.create(save_dir,recursive=TRUE,showWarnings=FALSE)
        write.csv(d,file.path(save_dir,'data.csv'),row.names=FALSE)
        set.seed(bootstrap_seed);idx<-matrix(sample.int(n,n*draws,TRUE),nrow=draws,byrow=TRUE)
        write.table(idx,file.path(save_dir,'indices.csv'),row.names=FALSE,col.names=FALSE,sep=',')
        jsonlite::write_json(c(q,list(draws=draws,seed=bootstrap_seed,generation_seed=generation_seed)),file.path(save_dir,'request.json'),auto_unbox=TRUE,pretty=TRUE,digits=16)
    }
    start<-proc.time()[3]
    ans<-tryCatch(pathlab_fit(d,q$graph,q$settings,draws=as.integer(draws),seed=bootstrap_seed,robust=q$robust,indices=idx),error=identity)
    status<-data.frame(scenario=name,n=n,replicate=replicate,generation_seed=generation_seed,bootstrap_seed=bootstrap_seed,
        success=!inherits(ans,'error'),code=if(inherits(ans,'error'))if(is.null(ans$code))'R_ERROR' else ans$code else 'OK',
        message=if(inherits(ans,'error'))conditionMessage(ans) else '',elapsed_seconds=unname(proc.time()[3]-start),
        draws_requested=draws,draws_successful=if(inherits(ans,'error'))NA else ans$bootstrap$successful,
        draws_failed=if(inherits(ans,'error'))NA else ans$bootstrap$failed,
        reference_coefficient_max_abs=NA_real_,reference_covariance_max_abs=NA_real_)
    if(inherits(ans,'error'))return(list(status=status,metrics=NULL))
    deltas<-independent_reference(d,name,ans$equations,q$robust)
    status$reference_coefficient_max_abs<-deltas[1];status$reference_covariance_max_abs<-deltas[2]
    allrows<-setNames(ans$estimands,vapply(ans$estimands,`[[`,'','id'))
    selected<-Filter(function(r)r$kind %in% c('specific_indirect','direct','linear_index','conditional_index','mixed_index','conditional_derivative','conditional_mixed_derivative') ||
        (r$kind=='condition_difference' && allrows[[r$effect_a]]$kind=='specific_indirect'),ans$estimands)
    metrics<-lapply(selected,function(r)data.frame(scenario=name,n=n,replicate=replicate,type='effect',target=r$id,
        kind=r$kind,condition=as.character(jsonlite::toJSON(r$condition,auto_unbox=TRUE)),
        truth=effect_truth(r,name,allrows),estimate=r$estimate,lower=r$lower,upper=r$upper))
    for(target in names(ans$equations)) {
        e<-ans$equations[[target]];f<-e$fit
        truth<-coefficient_truth(name,target,e$terms,e$basis_description)
        for(i in seq_along(truth))metrics[[length(metrics)+1L]]<-data.frame(scenario=name,n=n,replicate=replicate,type='coefficient',
            target=paste(target,names(f$coefficients)[i],sep='/'),kind='coefficient',condition='{}',truth=truth[i],estimate=f$coefficients[i],lower=f$lower[i],upper=f$upper[i])
    }
    if(name=='heteroscedastic')for(r in ans$slopes) {
        stopifnot(r$path_id=='p_x_m1')
        metrics[[length(metrics)+1L]]<-data.frame(scenario=name,n=n,replicate=replicate,type='slope',
            target=paste0('HC3_X_M1_W',r$condition$w),kind='HC3_slope',condition=as.character(jsonlite::toJSON(r$condition,auto_unbox=TRUE)),
            truth=.5,estimate=r$estimate,lower=r$lower,upper=r$upper)
    }
    if(!is.null(save_dir)) {
        # Keep full distribution for exact replay, and all reported interval rows.
        jsonlite::write_json(ans[c('estimands','bootstrap','accepted_draws','slopes')],file.path(save_dir,'r-result.json'),auto_unbox=TRUE,pretty=TRUE,digits=16)
        write.csv(ans$bootstrap_distribution,file.path(save_dir,'r-distribution.csv'),row.names=FALSE)
        saveRDS(ans,file.path(save_dir,'r-fit.rds'))
    }
    list(status=status,metrics=do.call(rbind,metrics))
}

if(length(args)) {
    name<-args[1];n<-as.integer(args[2]);first<-as.integer(args[3]);last<-as.integer(args[4]);draws<-as.integer(args[5]);out<-args[6]
    dir.create(out,recursive=TRUE,showWarnings=FALSE)
    prefix<-file.path(out,paste(name,n,first,last,sep='-'))
    statusfile<-paste0(prefix,'-status.csv');metricfile<-paste0(prefix,'-metrics.csv')
    stopifnot(!file.exists(statusfile),!file.exists(metricfile))
    for(i in seq.int(first,last)) {
        saved<-if(i==first && first==1L)file.path(out,paste(name,n,'replay',sep='-')) else NULL
        result<-run_rep(name,n,i,draws,saved)
        write.table(result$status,statusfile,sep=',',row.names=FALSE,col.names=!file.exists(statusfile),append=file.exists(statusfile),qmethod='double')
        if(!is.null(result$metrics))write.table(result$metrics,metricfile,sep=',',row.names=FALSE,col.names=!file.exists(metricfile),append=file.exists(metricfile),qmethod='double')
        cat(name,n,i,result$status$code,format(result$status$elapsed_seconds,digits=4),'seconds\n');flush.console()
    }
    jsonlite::write_json(list(completed=TRUE,scenario=name,n=n,first=first,last=last,draws=draws,source_revision=source$source_revision,R=R.version.string),paste0(prefix,'-complete.json'),auto_unbox=TRUE,pretty=TRUE)
}
