pl_cache_class<-function(name)if(exists(name,inherits=TRUE))get(name,inherits=TRUE) else getFromNamespace(name,"pathlab")
pl_cache_wire_options<-function(options) {
    # The Desktop engine supplies typed option protobufs. R-created options do
    # not populate this wire snapshot automatically, so initialize it before
    # testing the genuine Analysis .save/.load path.
    get("initProtoBuf",asNamespace("jmvcore"))()
    pack_many<-function(values) {
        pb<-RProtoBuf::new(RProtoBuf::P("jamovi.coms.AnalysisOptions"));pb$hasNames<-!is.null(names(values))
        if(isTRUE(pb$hasNames))pb$names<-names(values)
        for(value in as.list(values))pb$add("options",pack_one(value));pb
    }
    pack_one<-function(value) {
        pb<-RProtoBuf::new(RProtoBuf::P("jamovi.coms.AnalysisOption"))
        if(is.null(value))pb$o<-2L else if(is.list(value)||length(value)!=1L)pb$c<-pack_many(value)
        else if(is.logical(value))pb$o<-as.integer(value) else if(is.integer(value))pb$i<-value
        else if(is.numeric(value))pb$d<-value else pb$s<-value
        pb
    }
    values<-setNames(lapply(options$names,function(name) {
        value<-options$option(name)$value;type<-class(options$option(name))[1]
        if(type%in%c("OptionVariables","OptionArray"))as.list(value) else if(type=="OptionInteger")as.integer(value) else if(type=="OptionNumber")as.numeric(value) else value
    }),options$names)
    options$fromProtoBuf(pack_many(values));options
}
pl_cache_fixture<-function(name="first_stage") {
    root<-getOption("pathlab.root");folder<-if(is.null(root))testthat::test_path("..","fixtures") else file.path(root,"modules/pathlab/tests/fixtures")
    prefix<-file.path(folder,name)
    q<-jsonlite::fromJSON(paste0(prefix,"-request.json"),simplifyVector=FALSE)
    d<-read.csv(paste0(prefix,".csv"),check.names=FALSE,stringsAsFactors=FALSE)
    for(coding in q$settings$categorical)if(is.character(unlist(coding$levels)))d[[coding$node_id]]<-factor(d[[coding$node_id]],levels=unlist(coding$levels))
    roles<-function(role)vapply(Filter(function(n)n$role==role,q$graph$nodes),`[[`,"","column")
    edges<-setNames(q$graph$edges,vapply(q$graph$edges,`[[`,"","id"))
    o<-list(modelMode="general",x="x",y="y",generalMediators=roles("mediator"),generalModerators=roles("moderator"),generalCovariates=roles("covariate"),
        generalPaths=lapply(q$graph$edges,function(e)list(source=toupper(e$source),target=toupper(e$target))),
        generalModerations=lapply(q$graph$moderations,function(m) {
            e<-edges[[m$path_id]];mm<-toupper(unlist(m$moderator_ids))
            list(source=toupper(e$source),target=toupper(e$target),mod1=mm[1],mod2=if(length(mm)>1L)mm[2] else "none")
        }),categoricalCoding=lapply(q$settings$categorical,function(c)list(role=toupper(c$node_id),reference=as.character(c$reference))),
        outcomeFamily=q$settings$outcome_family,logisticEvent=if(q$settings$outcome_family=="logistic")"1" else "",draws=200L,seed=q$seed,robust=FALSE,confidence=95,
        showEffectPlot=TRUE,showInteractionPlot=TRUE,showJNPlot=TRUE,plotTarget=if(name=="plots/logistic")"Y" else "M1",plotModerator=if(name=="categorical-logistic/joint_logistic")"Z" else "W")
    for(p in q$settings$probes)o[[paste0("generalProbes",toupper(p$node_id))]]<-paste(unlist(p$values),collapse=",")
    list(data=d,options=o,request=q)
}
pl_cache_harness<-function(f=pl_cache_fixture(),counter=NULL) {
    h<-new.env(parent=emptyenv());h$data<-f$data
    if(is.null(counter)) {counter<-new.env(parent=emptyenv());counter$calls<-0L;counter$forbid<-FALSE}
    h$counter<-counter
    base<-pl_cache_class("pathsClass")
    spy<-R6::R6Class(inherit=base,private=list(.computeFit=function(data,spec,settings) {
        counter$calls<-counter$calls+1L
        if(counter$forbid)stop("A plot-only change attempted to refit/bootstrap")
        super$.computeFit(data,spec,settings)
    }))
    h$a<-spy$new(options=pl_cache_wire_options(do.call(pl_cache_class("pathsOptions")$new,f$options)),analysisId=1L)
    h$a$.setReadDatasetHeaderSource(function(vars)h$data[FALSE,unlist(vars),drop=FALSE])
    h$a$.setReadDatasetSource(function(vars) {
        d<-h$data[,unlist(vars),drop=FALSE]
        attr(d,"jmv-weights")<-attr(h$data,"jmv-weights",exact=TRUE);d
    })
    h
}
pl_cache_change<-function(a,values) {
    for(name in names(values)) {o<-a$options$option(name);o$value<-values[[name]];env<-a$options$values();env[[name]]<-o$value}
    a$optionsChangedHandler(names(values))
}
pl_cache_json<-function(a)jsonlite::fromJSON(a$results$reproduction$content,simplifyVector=FALSE)
pl_cache_science<-function(r)r[setdiff(names(r),c("plots","plot_conditions","fit_cache"))]
