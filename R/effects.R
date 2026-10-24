# Underscore-free moderator IDs retain the preview's historical identity.
# When an ID contains the historical separator, use a length-prefixed token
# in a namespace (~ and :) which valid graph identifiers cannot contain.
pl_derivative_key <- function(ids) {
    ids<-sort(ids,method="radix")
    if(any(grepl("_",ids,fixed=TRUE))) paste0("~",paste(nchar(ids,type="bytes"),ids,sep=":",collapse="~")) else paste(ids,collapse="_")
}

pl_route <- function(path,model,equations,betas,condition,derivative=character(),focal_factor=NULL) {
    if(!length(path)) return(0)
    assignments<-if(length(derivative)) as.matrix(expand.grid(rep(list(seq_along(path)),length(derivative)),KEEP.OUT.ATTRS=FALSE)) else matrix(integer(),1,0)
    sum(vapply(seq_len(nrow(assignments)),function(j) {
        prod(vapply(seq_along(path),function(k) {
            e<-model$edges[[path[k]]];eq<-equations[[e$target]]
            factor<-if(e$source==model$x && !is.null(focal_factor))focal_factor else eq$basis[[e$source]][1]
            v<-pl_slope_vector(e,eq,condition,derivative[which(assignments[j,]==k)],factor)
            sum(v*betas[[e$target]])
        },0.0))
    },0.0))
}

#' Evaluate specific and summed associations and degree-aware derivatives
#' @export
evaluate_estimands <- function(model,sample,equations,betas=lapply(equations,function(e)e$fit$coefficients)) {
    rows<-list();constant_indices<-list()
    # Contrasts share the same coefficient draw as their source estimands.
    contrast<-function(a,b,kind) {
        row<-list(id=paste("contrast",a$id,b$id,sep="_"),kind=kind,path_ids=a$path_ids,condition=a$condition,condition_b=b$condition,effect_a=a$id,effect_b=b$id,estimate=a$estimate-b$estimate)
        if(!is.null(a$focal_factor)){row$focal_factor<-a$focal_factor;row$focal_contrast<-a$focal_contrast}
        rows[[length(rows)+1L]]<<-row
    }
    indirect<-Filter(function(r)length(r$path_ids)>1,model$routes)
    direct<-Filter(function(r)length(r$path_ids)==1,model$routes)
    numericmods<-setdiff(model$moderators,names(model$coding))
    derivatives<-unlist(lapply(seq_along(numericmods),function(k)combn(numericmods,k,simplify=FALSE)),recursive=FALSE)
    focal_basis<-sample$basis[[model$x]];categorical_x<-model$x %in% names(model$coding)
    for(fi in seq_along(focal_basis)) {
    focal_factor<-focal_basis[fi];effects<-list()
    # Continuous models retain their historical row identifiers exactly.
    focal_key<-function(prefix)if(categorical_x)paste0(prefix,"_",fi) else prefix
    for(ci in seq_along(sample$conditions)) {
        cond<-sample$conditions[[ci]]; rr<-list()
        add<-function(id,kind,path,value,condition=cond,moderators=character(),effect_kind=kind) {
            row<-list(id=id,kind=kind,path_ids=path,condition=condition,moderator_ids=moderators,effect_kind=effect_kind,estimate=value)
            if(categorical_x){row$focal_factor<-focal_factor;row$focal_contrast<-sample$basis_description[[focal_factor]]}
            rows[[length(rows)+1L]]<<-row; row
        }
        values<-vapply(indirect,function(r)pl_route(r$path_ids,model,equations,betas,cond,focal_factor=focal_factor),0.0)
        for(ri in seq_along(indirect)) rr[[length(rr)+1L]]<-add(paste0(focal_key("effect"),"_",ci,"_route_",ri),"specific_indirect",indirect[[ri]]$path_ids,values[ri])
        d<-if(length(direct))pl_route(direct[[1]]$path_ids,model,equations,betas,cond,focal_factor=focal_factor) else 0
        for(k in c("total_indirect","direct","total")) rr[[length(rr)+1L]]<-add(paste0(focal_key("effect"),"_",ci,"_",k),k,character(),switch(k,total_indirect=sum(values),direct=d,total=sum(values)+d))
        effects[[ci]]<-rr
        for(der in derivatives) {
            degrees<-lapply(indirect,function(r)setNames(vapply(model$moderators,function(i)sum(vapply(r$path_ids,function(p) {
                e<-model$edges[[p]];eq<-equations[[e$target]]
                source_factor<-if(e$source==model$x)focal_factor else eq$basis[[e$source]][1]
                any(vapply(eq$terms,function(t)source_factor %in% t && any(t %in% eq$basis[[i]]),TRUE))
            },TRUE)),0L),model$moderators))
            if(!length(degrees) || !any(vapply(degrees,function(d)all(d[der]>0),TRUE))) next
            vals<-vapply(indirect,function(r)pl_route(r$path_ids,model,equations,betas,cond,der,focal_factor),0.0)
            groups<-c(degrees,list(Reduce(pmax,degrees))); vv<-c(vals,sum(vals))
            route_indices<-list()
            for(ri in seq_along(groups)) {
                deg<-groups[[ri]]; constant<-all(deg[der]<=1) && all(deg[setdiff(model$moderators,der)]==0)
                kind<-if(length(der)==1)if(constant)"linear_index" else if(deg[der]<=1)"conditional_index" else "conditional_derivative" else if(constant)"mixed_index" else "conditional_mixed_derivative"
                path<-if(ri<=length(indirect))indirect[[ri]]$path_ids else character()
                id<-paste(focal_key("index"),if(constant)0 else ci,ri,pl_derivative_key(der),sep="_")
                if(constant && ci>1) row<-constant_indices[[id]] else {
                    row<-add(id,kind,path,vv[ri],if(constant)list() else cond,der,if(ri<=length(indirect))"specific_indirect" else "total_indirect")
                    if(constant) constant_indices[[id]]<-row
                }
                if(ri<=length(indirect)) route_indices[[length(route_indices)+1L]]<-row
            }
            # A constant route index still participates in comparisons with a
            # conditional derivative at every probe. Display the index once,
            # but retain its identity and joint draw for these comparisons.
            if(length(route_indices)>1) for(pair in combn(seq_along(route_indices),2,simplify=FALSE)) {
                a<-route_indices[[pair[1]]]; b<-route_indices[[pair[2]]]
                if(ci>1 && !length(a$condition) && !length(b$condition)) next
                contrast(a,b,"route_derivative_difference")
            }
        }
    }
    # All contrasts are evaluated within a joint replication, never from CI overlap.
    for(rr in effects) {
        ss<-Filter(function(r)r$kind=="specific_indirect",rr)
        if(length(ss)>1) for(pair in combn(seq_along(ss),2,simplify=FALSE)) contrast(ss[[pair[1]]],ss[[pair[2]]],"route_difference")
    }
    if(length(effects)>1) for(pair in combn(seq_along(effects),2,simplify=FALSE)) for(k in seq_along(effects[[pair[1]]])) contrast(effects[[pair[1]]][[k]],effects[[pair[2]]][[k]],"condition_difference")
    }
    pl_require(length(rows)<=8000,"RESOURCE_LIMIT")
    rows
}
