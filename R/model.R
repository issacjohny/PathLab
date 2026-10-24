# Independently written graph compiler. Never evaluates user-supplied R syntax.
pl_error <- function(code, detail=code) stop(structure(list(message=paste(code, detail, sep=": "), call=NULL, code=code), class=c("pathlab_error","error","condition")))
pl_require <- function(ok, code="INVALID_MODEL", detail=code) if (!isTRUE(ok)) pl_error(code, detail)
pl_keys <- function(x, keys) is.list(x) && setequal(names(x),keys) && length(names(x))==length(keys)
pl_ids <- function(x) vapply(x, `[[`, "", "id")
pl_product <- function(x) if (length(x)) Reduce(`*`,x) else 1
pl_category_scalar <- function(x) length(x)==1L && !is.na(x) && ((is.character(x) && nchar(x,type="chars")>=1L && nchar(x,type="chars")<=120L) || (is.numeric(x) && !is.logical(x) && is.finite(x)))

#' Validate a versioned observed-variable model
#' @export
validate_spec <- function(spec, settings, columns) {
    pl_require(pl_keys(spec,c("schema_version","nodes","edges","moderations")) && identical(as.numeric(spec$schema_version),1))
    nodes <- spec$nodes; edges <- spec$edges; mods <- spec$moderations
    pl_require(length(nodes)>=3 && length(nodes)<=35 && length(edges)>=1 && length(edges)<=150 && length(mods)<=150)
    ident <- function(x) is.character(x) && length(x)==1 && !is.na(x) && grepl("^[A-Za-z][A-Za-z0-9_]{0,39}$",x) && x!="STRAT_GROUP"
    for(n in nodes) pl_require(pl_keys(n,c("id","column","role","measurement_scale")) && ident(n$id) && n$column %in% columns && n$role %in% c("independent","dependent","mediator","moderator","covariate") && n$measurement_scale %in% c("nominal","ordinal","interval","ratio"))
    ids <- pl_ids(nodes); by <- setNames(nodes,ids)
    role <- vapply(nodes,`[[`,"","role"); count <- function(r) sum(role==r)
    pl_require(!anyDuplicated(ids) && !anyDuplicated(vapply(nodes,`[[`,"","column")) && count("independent")==1 && count("dependent")==1 && count("mediator")<=10 && count("covariate")<=20)
    version <- if(is.null(settings$schema_version)) 1 else settings$schema_version
    legacy <- c("outcome_family","categorical","probes")
    pl_require(version %in% 1:3 && pl_keys(settings,if(version==1) legacy else c(legacy,"schema_version","automatic_probes","bootstrap_interval_quantile_method")),"INVALID_OPTIONS")
    pl_require(count("moderator") <= if(version==3) 3 else 2)
    if(version>=2) {
        p <- settings$automatic_probes
        pl_require(pl_keys(p,c("probabilities","quantile_method","sample","resolve_before_bootstrap")) && is.numeric(unlist(p$probabilities)) && length(p$probabilities)%in%1:5 && all(is.finite(unlist(p$probabilities))) && all(unlist(p$probabilities)>=0 & unlist(p$probabilities)<=1) && all(diff(unlist(p$probabilities))>0) && p$quantile_method %in% c("linear","weibull") && identical(p$sample,"fixed_complete_case_sample") && isTRUE(p$resolve_before_bootstrap) && identical(settings$bootstrap_interval_quantile_method,"linear"),"INVALID_OPTIONS")
    }
    pl_require(length(settings$outcome_family)==1L && settings$outcome_family %in% c("linear","logistic"),"INVALID_OPTIONS")
    pl_require(is.list(settings$categorical) && length(settings$categorical)<=21L+count("moderator"),"INVALID_OPTIONS")
    coding<-list()
    for(entry in settings$categorical) {
        pl_require(pl_keys(entry,c("node_id","levels","reference")) && is.character(entry$node_id) && length(entry$node_id)==1L && entry$node_id %in% ids && !(entry$node_id %in% names(coding)) && by[[entry$node_id]]$role %in% c("independent","moderator","covariate"),"INVALID_OPTIONS","Categorical declarations are allowed only for predictors, moderators and covariates")
        levels<-as.list(entry$levels)
        pl_require(length(levels)>=2L && length(levels)<=10L && all(vapply(levels,pl_category_scalar,TRUE)) && length(unique(vapply(levels,is.character,TRUE)))==1L && !anyDuplicated(unlist(levels)) && pl_category_scalar(entry$reference) && identical(is.character(entry$reference),is.character(levels[[1]])) && entry$reference %in% unlist(levels),"INVALID_OPTIONS","Declare two to ten distinct, consistently typed levels and a reference among those levels")
        coding[[entry$node_id]]<-entry
    }
    pl_require(all(vapply(nodes,function(n)n$measurement_scale!="nominal" || !(n$role %in% c("independent","moderator","covariate")) || n$id %in% names(coding),TRUE)),"INVALID_OPTIONS","Each nominal exogenous variable needs explicit categorical levels and a reference")
    pl_require(!any(vapply(nodes,function(n)n$measurement_scale=="nominal" && n$role=="mediator",TRUE)),"INVALID_OPTIONS","Categorical mediators are not supported")
    pl_require(!any(vapply(nodes,function(n)n$measurement_scale=="nominal" && n$role=="dependent",TRUE)) || settings$outcome_family=="logistic","INVALID_OPTIONS","A nominal outcome requires the binary logistic family")
    for(e in edges) pl_require(pl_keys(e,c("id","source","target")) && ident(e$id) && e$source %in% ids && e$target %in% ids && e$source!=e$target && by[[e$target]]$role %in% c("mediator","dependent") && by[[e$source]]$role!="dependent")
    edgeids <- pl_ids(edges); be <- setNames(edges,edgeids)
    pl_require(!anyDuplicated(edgeids) && !anyDuplicated(vapply(edges,function(e) paste(e$source,e$target),"")))
    signatures <- character(); modids <- character(); used <- character()
    for(m in mods) {
        pl_require(pl_keys(m,c("id","path_id","moderator_ids")) && ident(m$id) && !(m$id %in% modids) && m$path_id %in% edgeids && length(m$moderator_ids)%in%1:2)
        mm <- unlist(m$moderator_ids); e <- be[[m$path_id]]
        pl_require(is.character(mm) && !anyDuplicated(mm) && all(mm %in% ids) && all(vapply(mm,function(i) i!=e$source && i!=e$target && by[[i]]$role=="moderator" && any(vapply(edges,function(p) p$source==i && p$target==e$target,TRUE)),TRUE)))
        sig <- paste(e$target,paste(sort(c(e$source,mm)),collapse=":"),sep="|")
        pl_require(!(sig %in% signatures),detail="Duplicate interaction terms are not allowed, including equivalent attachments to a different source arrow"); signatures<-c(signatures,sig); modids<-c(modids,m$id); used<-c(used,mm)
    }
    for(m in mods) if(length(m$moderator_ids)==2) {
        e <- be[[m$path_id]]; f <- sort(c(e$source,unlist(m$moderator_ids)))
        lower <- apply(combn(f,2),2,function(v) paste(e$target,paste(v,collapse=":"),sep="|"))
        pl_require(all(lower %in% signatures),detail="Joint interactions require every lower-order interaction")
    }
    moderators <- ids[role=="moderator"]
    pl_require(setequal(used,moderators),detail="Every assigned moderator must be used in at least one interaction row; explicitly clear unused moderator roles")
    pl_require(count("mediator")>0 || length(mods)>0,detail="A model needs at least one mediator or a moderated arrow")
    pl_require(setequal(unlist(lapply(edges,function(e)c(e$source,e$target))),ids),detail="Every assigned variable must appear in an explicit arrow")
    visiting <- character(); visited <- character()
    visit <- function(i) {
        pl_require(!(i %in% visiting),detail="Directed cycles are not allowed")
        if(i %in% visited) return(invisible(NULL))
        visiting <<- c(visiting,i)
        for(e in edges) if(e$source==i) visit(e$target)
        visiting <<- setdiff(visiting,i); visited <<- c(visited,i)
    }
    for(i in ids) visit(i)
    x <- ids[role=="independent"]; y <- ids[role=="dependent"]
    routes <- list()
    walk <- function(i,path,names) {
        if(i==y) {
            routes[[length(routes)+1L]] <<- list(path_ids=path,node_ids=names)
            pl_require(length(routes)<=100,"RESOURCE_LIMIT"); return(invisible(NULL))
        }
        for(e in edges[order(edgeids)]) if(e$source==i) walk(e$target,c(path,e$id),c(names,e$target))
    }
    walk(x,character(),x)
    pl_require(length(routes)>0 && all(ids[role=="mediator"] %in% unlist(lapply(routes,`[[`,"node_ids"))))
    seen <- character()
    pl_require(is.list(settings$probes),"INVALID_OPTIONS")
    for(p in settings$probes) {
        v<-as.list(p$values)
        pl_require(pl_keys(p,c("node_id","values")) && p$node_id %in% moderators && !(p$node_id %in% seen) && length(v)%in%1:5 && all(vapply(v,pl_category_scalar,TRUE)) && !anyDuplicated(unlist(v)),"INVALID_OPTIONS")
        if(p$node_id %in% names(coding)) {
            levels<-as.list(coding[[p$node_id]]$levels)
            pl_require(all(vapply(v,function(z)identical(is.character(z),is.character(levels[[1]])) && z %in% unlist(levels),TRUE)),"INVALID_OPTIONS","Categorical probes must be declared levels with their original type")
        } else pl_require(all(vapply(v,is.numeric,TRUE)),"INVALID_OPTIONS")
        seen <- c(seen,p$node_id)
    }
    list(spec=spec,settings=settings,by=by,edges=be,x=x,y=y,moderators=moderators,numerical_moderators=setdiff(moderators,names(coding)),coding=coding,routes=routes,version=version)
}

#' Prepare one common complete-case sample without transformations
#' @export
prepare_sample <- function(data, model) {
    pl_require(is.data.frame(data) && !anyDuplicated(names(data)),"INVALID_DATA")
    cols <- vapply(model$by,`[[`,"","column")
    keep <- complete.cases(data[,cols,drop=FALSE]); d <- data[keep,cols,drop=FALSE]
    pl_require(nrow(d)>=10,"INSUFFICIENT_DATA")
    values<-basis<-basis_description<-list();coding_rows<-list()
    for(i in names(model$by)) {
        node<-model$by[[i]];v<-d[[node$column]]
        if(i %in% names(model$coding)) {
            declaration<-model$coding[[i]];levels<-unlist(declaration$levels);reference<-declaration$reference
            if(is.character(levels)) {
                pl_require(is.character(v) || is.factor(v),"INVALID_OPTIONS","Text levels require a text or factor column")
                v<-as.character(v)
            } else pl_require(is.numeric(v) && !is.factor(v),"NUMERIC_TYPE_MISMATCH")
            pl_require(setequal(unique(v),levels),"INVALID_OPTIONS","Every declared categorical level must occur in the fixed complete-case sample, with no undeclared levels")
            basis[[i]]<-character()
            for(k in seq_along(levels)) if(levels[k]!=reference) {
                factor<-paste0(i,"@",k-1L);basis[[i]]<-c(basis[[i]],factor)
                values[[factor]]<-as.numeric(v==levels[k])
                basis_description[[factor]]<-list(node_id=i,column=node$column,level=unname(levels[k]),reference=reference)
            }
            coding_rows[[length(coding_rows)+1L]]<-list(node_id=i,column=node$column,levels=levels,reference=reference,method="indicator",counts=lapply(levels,function(level)list(level=unname(level),n=sum(v==level))))
        } else {
            pl_require(is.numeric(v) && !is.factor(v),"NUMERIC_TYPE_MISMATCH")
            pl_require(all(is.finite(v)),"INVALID_DATA")
            pl_require(length(unique(v))>1,"CONSTANT_VARIABLE")
            basis[[i]]<-i;values[[i]]<-as.numeric(v)
            basis_description[[i]]<-list(node_id=i,column=node$column)
        }
    }
    if(model$settings$outcome_family=="logistic")pl_require(setequal(unique(values[[model$y]]),c(0,1)),"INVALID_OPTIONS","A logistic outcome must contain both numerical values 0 and 1")
    probes<-list(); explicit<-setNames(lapply(model$settings$probes,`[[`,"values"),vapply(model$settings$probes,`[[`,"","node_id"))
    policy<-model$settings$automatic_probes
    for(i in model$moderators) probes[[i]]<-if(i %in% names(explicit)) unlist(explicit[[i]]) else if(i %in% names(model$coding))unlist(model$coding[[i]]$levels) else unique(as.numeric(quantile(values[[i]],if(is.null(policy))c(.16,.5,.84) else unlist(policy$probabilities),type=if(!is.null(policy) && policy$quantile_method=="weibull")6 else 7,names=FALSE)))
    pl_require(prod(lengths(probes))<=25,"RESOURCE_LIMIT","At most 25 moderator probe combinations are supported. Supply fewer custom values for one or more moderators (three automatic probes for each of three moderators would produce 27 combinations).")
    conditions <- if(length(probes)) lapply(seq_len(nrow(g<-expand.grid(probes,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE))),function(i) as.list(g[i,,drop=FALSE])) else list(list())
    exclusions <- lapply(which(!keep),function(i) list(source_row=i,missing_node_ids=names(model$by)[vapply(model$by,function(n)is.na(data[[n$column]][i]),TRUE)]))
    list(values=values,basis=basis,basis_description=basis_description,coding_rows=coding_rows,n=nrow(d),source_n=nrow(data),included_rows=which(keep),exclusions=exclusions,probes=probes,conditions=conditions,
         extrapolation=names(probes)[vapply(names(probes),function(i)!(i %in% names(model$coding)) && any(probes[[i]]<min(values[[i]]) | probes[[i]]>max(values[[i]])),TRUE)])
}

#' Compile precisely the declared arrows and interaction terms
#' @export
compile_equations <- function(model, sample) {
    terms <- list()
    add <- function(target,factors) {
        factors<-sort(factors); key<-paste(factors,collapse=":")
        terms[[target]][[key]] <<- factors
    }
    for(e in model$spec$edges)for(factor in sample$basis[[e$source]])add(e$target,factor)
    for(m in model$spec$moderations) {
        e<-model$edges[[m$path_id]];combination<-expand.grid(sample$basis[c(e$source,unlist(m$moderator_ids))],KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
        for(i in seq_len(nrow(combination)))add(e$target,unname(unlist(combination[i,,drop=FALSE])))
    }
    pl_require(sample$n*sum(lengths(terms)+1L)<=5e6,"RESOURCE_LIMIT","The declared equation designs exceed five million matrix cells")
    out<-list()
    for(target in sort(names(terms))) {
        t<-terms[[target]]; t<-c(list(character()),t[order(lengths(t),names(t))]); names(t)[1]<-"(Intercept)"
        pl_require(length(t)<=80,"RESOURCE_LIMIT")
        X<-vapply(t,function(f) if(length(f)) pl_product(sample$values[f]) else rep(1,sample$n),numeric(sample$n))
        labels<-vapply(t,function(f)if(length(f))paste(vapply(f,function(i){description<-sample$basis_description[[i]];paste0(description$column,if(!is.null(description$level))paste0(" [",pl_value_label(description$level)," vs ",pl_value_label(description$reference),"]") else "")},""),collapse=" * ") else "1","")
        means<-lapply(sample$values[setdiff(names(model$by),names(model$coding))],mean)
        family<-if(target==model$y)model$settings$outcome_family else "linear"
        lhs<-if(family=="logistic")paste0("logit[P(",model$by[[target]]$column," = 1)]") else model$by[[target]]$column
        out[[target]]<-list(target_id=target,terms=t,design=X,y=sample$values[[target]],basis=sample$basis,basis_description=sample$basis_description,coding=model$coding,means=means,family=family,equation=paste(lhs,"~",paste(labels,collapse=" + ")))
    }
    out
}
