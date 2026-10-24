# Compile only fixed graph/probe work. The public evaluator remains the oracle:
# this plan preserves its row metadata, route assignment order and arithmetic.
pl_compile_estimand_plan <- function(model,sample,equations,original) {
    indirect<-Filter(function(r)length(r$path_ids)>1,model$routes)
    direct<-Filter(function(r)length(r$path_ids)==1,model$routes)
    slopes<-list(); routes<-list(); rows<-vector("list",length(original))
    slope_ids<-new.env(hash=TRUE,parent=emptyenv())
    route_ids<-new.env(hash=TRUE,parent=emptyenv())
    ids<-vapply(original,`[[`,"","id")
    slope_id<-function(path,ci,derivative,focal_factor) {
        key<-paste(path,ci,paste(derivative,collapse=":"),focal_factor,sep="~")
        if(exists(key,envir=slope_ids,inherits=FALSE))return(slope_ids[[key]])
        e<-model$edges[[path]]
        value<-length(slopes)+1L
        eq<-equations[[e$target]];factor<-if(e$source==model$x)focal_factor else eq$basis[[e$source]][1]
        slopes[[value]]<<-list(target=e$target,vector=pl_slope_vector(e,eq,sample$conditions[[ci]],derivative,factor))
        slope_ids[[key]]<-value
        value
    }
    route_id<-function(path,ci,derivative=character(),focal_factor=sample$basis[[model$x]][1]) {
        key<-paste(paste(path,collapse=":"),ci,paste(derivative,collapse=":"),focal_factor,sep="~")
        if(exists(key,envir=route_ids,inherits=FALSE))return(route_ids[[key]])
        assignments<-if(length(derivative))as.matrix(expand.grid(rep(list(seq_along(path)),length(derivative)),KEEP.OUT.ATTRS=FALSE)) else matrix(integer(),1,0)
        recipe<-matrix(0L,nrow(assignments),length(path))
        for(j in seq_len(nrow(assignments)))for(k in seq_along(path))
            recipe[j,k]<-slope_id(path[k],ci,derivative[which(assignments[j,]==k)],focal_factor)
        value<-length(routes)+1L
        routes[[value]]<<-recipe
        route_ids[[key]]<-value
        value
    }
    for(i in seq_along(original)) {
        row<-original[[i]]
        if(!is.null(row$effect_a)) {
            pair<-match(c(row$effect_a,row$effect_b),ids)
            pl_require(!anyNA(pair) && all(pair<i),"NUMERICAL_FAILURE","Invalid estimand contrast dependency")
            rows[[i]]<-list(kind="contrast",indices=pair)
            next
        }
        ci<-if(!length(row$condition))1L else which(vapply(sample$conditions,function(c)identical(c,row$condition),TRUE))[1]
        pl_require(length(ci)==1 && !is.na(ci),"NUMERICAL_FAILURE","Unknown fixed estimand condition")
        derivative<-row$moderator_ids
        focal_factor<-if(is.null(row$focal_factor))sample$basis[[model$x]][1] else row$focal_factor
        kind<-if(length(derivative))row$effect_kind else row$kind
        if(kind=="specific_indirect") {
            rows[[i]]<-list(kind="route",indices=route_id(row$path_ids,ci,derivative,focal_factor))
        } else if(kind %in% c("total_indirect","total")) {
            rr<-vapply(indirect,function(r)route_id(r$path_ids,ci,derivative,focal_factor),0L)
            dd<-if(kind=="total" && length(direct))route_id(direct[[1]]$path_ids,ci,focal_factor=focal_factor) else 0L
            rows[[i]]<-list(kind=kind,indices=rr,direct=dd)
        } else if(kind=="direct") {
            rows[[i]]<-list(kind="route",indices=if(length(direct))route_id(direct[[1]]$path_ids,ci,focal_factor=focal_factor) else 0L)
        } else pl_error("NUMERICAL_FAILURE","Unknown estimand recipe")
    }
    list(slopes=slopes,routes=routes,rows=rows)
}

pl_evaluate_estimand_plan <- function(plan,betas) {
    slopes<-vapply(plan$slopes,function(s)sum(s$vector*betas[[s$target]]),0.0)
    routes<-vapply(plan$routes,function(recipe)sum(vapply(seq_len(nrow(recipe)),function(j)prod(slopes[recipe[j,]]),0.0)),0.0)
    values<-numeric(length(plan$rows))
    for(i in seq_along(plan$rows)) {
        row<-plan$rows[[i]]
        values[i]<-switch(row$kind,
            contrast=values[row$indices[1]]-values[row$indices[2]],
            route=if(row$indices==0L)0 else routes[row$indices],
            total_indirect=sum(routes[row$indices]),
            total=sum(routes[row$indices])+if(row$direct==0L)0 else routes[row$direct])
    }
    values
}
