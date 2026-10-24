# Human-readable native output, compiled from the same fitted graph.
pl_node_label <- function(spec,id) {
    if(!length(id))return("")
    by<-setNames(spec$nodes,pl_ids(spec$nodes))
    vapply(id,function(i)if(i %in% names(by))by[[i]]$column else i,"")
}
pl_value_label <- function(value)as.character(jsonlite::toJSON(value,auto_unbox=TRUE,digits=NA))
pl_term_label <- function(spec,term,descriptions=NULL) {
    if(identical(term,"(Intercept)"))return("Intercept")
    paste(vapply(strsplit(term,":",fixed=TRUE)[[1]],function(id) {
        description<-descriptions[[id]]
        if(is.null(description))return(pl_node_label(spec,id))
        paste0(description$column,if(!is.null(description$level))paste0(" [",pl_value_label(description$level)," vs ",pl_value_label(description$reference),"]") else "")
    },""),collapse=" \u00d7 ")
}
pl_path_label <- function(spec,path) {
    if(!length(path))return("")
    by<-setNames(spec$edges,pl_ids(spec$edges))
    if(all(path %in% names(by))) {
        edges<-by[path]; ids<-c(edges[[1]]$source,vapply(edges,`[[`,"","target"))
        paste(pl_node_label(spec,ids),collapse=" \u2192 ")
    } else paste(path,collapse=", ")
}
pl_condition_label <- function(spec,condition) {
    if(!length(condition))return("Constant / unconditional")
    by<-setNames(spec$nodes,pl_ids(spec$nodes))
    values<-vapply(names(condition),function(id) {
        value<-condition[[id]]
        if(is.numeric(value) && !identical(by[[id]]$measurement_scale,"nominal"))trimws(formatC(value,digits=4,format="fg")) else pl_value_label(value)
    },"")
    paste(pl_node_label(spec,names(condition)),values,sep=" = ",collapse=", ")
}
pl_focal_label <- function(label,contrast) {
    if(is.null(contrast))return(label)
    paste0(label," [",contrast$column,": ",pl_value_label(contrast$level)," vs ",pl_value_label(contrast$reference),"]")
}
pl_association_label <- function(row) {
    labels<-c(specific_indirect="Specific indirect",total_indirect="Total indirect",direct="Direct",total="Total path association",linear_index="Linear index",mixed_index="Mixed index",conditional_index="Conditional index",conditional_derivative="Conditional derivative",conditional_mixed_derivative="Conditional mixed derivative",route_difference="Indirect-route difference",condition_difference="Probe difference",route_derivative_difference="Route-derivative difference")
    if(row$kind %in% names(labels))unname(labels[row$kind]) else row$kind
}
pl_estimand_label <- function(spec,row,rows=NULL,lookup=NULL) {
    label<-function(r) {
        indexed<-length(r$moderator_ids)>0
        base<-if(length(r$path_ids))pl_path_label(spec,r$path_ids) else if(indexed)pl_association_label(list(kind=r$effect_kind)) else pl_association_label(r)
        base<-pl_focal_label(base,r$focal_contrast)
        if(!indexed)return(base)
        moderators<-pl_node_label(spec,unlist(r$moderator_ids,use.names=FALSE))
        derivative<-if(length(moderators)==1)paste("d/d",moderators) else paste0("d",length(moderators),"/(",paste("d",moderators,collapse=" "),")")
        paste0(base," [",derivative,"]")
    }
    if(!is.null(row$effect_a)) {
        if(is.null(lookup))lookup<-setNames(rows,vapply(rows,`[[`,"","id"))
        first<-lookup[[row$effect_a]]; second<-lookup[[row$effect_b]]
        return(paste(label(first),"minus",label(second)))
    }
    label(row)
}

pl_effect_values <- function(spec,row,lookup) {
    list(kind=pl_association_label(row),route=pl_estimand_label(spec,row,lookup=lookup),
      condition=paste(pl_condition_label(spec,row$condition),if(length(row$condition_b))paste("minus",pl_condition_label(spec,row$condition_b))),
      estimate=row$estimate,se=row$bootstrap_se,lower=row$lower,upper=row$upper)
}

# Stock Table$addRow rebuilds all accumulated JSON row names on every append.
# Bound that work with ordinary all-visible native tables; every row and key is
# retained in the original order, using only public Array/Table lifecycle APIs.
pl_populate_effect_pages <- function(array,spec,rows,scale=NULL,checkpoint=NULL) {
    array$clear()
    if(!length(rows))return(invisible(array))
    lookup<-setNames(rows,vapply(rows,`[[`,"","id"))
    for(page in seq_len(ceiling(length(rows)/50))) {
        if(!is.null(checkpoint))checkpoint()
        first<-(page-1L)*50L+1L;last<-min(page*50L,length(rows))
        table<-array$addItem(page)
        title<-if(is.null(scale))"Effects and contrasts - rows" else paste("Effects and contrasts -",scale,"- rows")
        table$setTitle(paste(title,first,"to",last,"of",length(rows)))
        for(i in first:last)table$addRow(rowKey=rows[[i]]$id,values=pl_effect_values(spec,rows[[i]],lookup))
    }
    invisible(array)
}

pl_html_escape <- function(s) {
    s<-gsub("&","&amp;",s,fixed=TRUE); s<-gsub("<","&lt;",s,fixed=TRUE); s<-gsub(">","&gt;",s,fixed=TRUE)
    s<-gsub('"',"&quot;",s,fixed=TRUE); gsub("'","&#39;",s,fixed=TRUE)
}
pl_diagram_layout <- function(spec) {
    roles<-vapply(spec$nodes,function(n)n$role,""); ids<-pl_ids(spec$nodes)
    mediators<-ids[roles=="mediator"]; chain<-c(ids[roles=="independent"],mediators,ids[roles=="dependent"])
    mods<-ids[roles=="moderator"]; covariates<-ids[roles=="covariate"]
    width<-max(560,150*length(chain),120*(length(mods)+1),120*(length(covariates)+1))
    height<-if(length(covariates))360 else 260
    pos<-setNames(lapply(seq_along(chain),function(k)c(70+150*(k-1),160)),chain)
    for(k in seq_along(mods))pos[[mods[k]]]<-c(150*k,40)
    cov_x<-if(length(covariates)==1)width/2 else if(length(covariates))seq(70,width-70,length.out=length(covariates)) else numeric()
    for(k in seq_along(covariates))pos[[covariates[k]]]<-c(cov_x[k],285)
    pl_require(setequal(names(pos),ids),"INVALID_MODEL","Every diagram node must have a supported graph role")
    # Intersect a ray with the same rectangle used for the visible node.
    anchor<-function(from,to) {
        delta<-to-from; active<-abs(delta)>0
        from+delta*min(c(46,18)[active]/abs(delta[active]))
    }
    edges<-setNames(lapply(spec$edges,function(e) {
        a<-pos[[e$source]]; b<-pos[[e$target]]
        if(a[2]==b[2] && abs(b[1]-a[1])>170) {
            bend<-if(e$source==chain[1] && e$target==tail(chain,1))245 else 90
            control<-c((a[1]+b[1])/2,bend)
            from<-anchor(a,control);to<-anchor(b,control)
            list(from=from,to=to,control=control,midpoint=.25*from+.5*control+.25*to)
        } else {
            from<-anchor(a,b);to<-anchor(b,a)
            list(from=from,to=to,midpoint=(from+to)/2)
        }
    }),pl_ids(spec$edges))
    list(width=width,height=height,positions=pos,edges=edges)
}
pl_diagram <- function(spec) {
    layout<-pl_diagram_layout(spec);pos<-layout$positions
    svg<-c(sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%s" height="%s" viewBox="0 0 %s %s" role="img" aria-label="Specified PathLab graph">',layout$width,layout$height,layout$width,layout$height),
        '<defs><marker id="plArrow" markerWidth="8" markerHeight="8" refX="7" refY="4" orient="auto"><path d="M0,0 L8,4 L0,8" fill="#34546d"/></marker></defs>')
    for(e in spec$edges) {
        geometry<-layout$edges[[e$id]];a<-geometry$from;b<-geometry$to
        attributes<-sprintf('data-path-id="%s" fill="none" stroke="#34546d" stroke-width="1.6" marker-end="url(#plArrow)"',pl_html_escape(e$id))
        if(length(geometry$control)) svg<-c(svg,sprintf('<path d="M%s,%s Q%s,%s %s,%s" %s/>',a[1],a[2],geometry$control[1],geometry$control[2],b[1],b[2],attributes))
        else svg<-c(svg,sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" %s/>',a[1],a[2],b[1],b[2],attributes))
    }
    for(m in spec$moderations) {
        midpoint<-layout$edges[[m$path_id]]$midpoint
        for(i in unlist(m$moderator_ids)) {
            delta<-midpoint-pos[[i]];active<-abs(delta)>0
            from<-pos[[i]]+delta*min(c(46,18)[active]/abs(delta[active]))
            svg<-c(svg,sprintf('<line data-moderation-id="%s" x1="%s" y1="%s" x2="%s" y2="%s" stroke="#a55c32" stroke-dasharray="5,4" stroke-width="1.6"/>',pl_html_escape(m$id),from[1],from[2],midpoint[1],midpoint[2]))
        }
    }
    for(i in names(pos)) {
        p<-pos[[i]];label<-pl_html_escape(pl_node_label(spec,i))
        svg<-c(svg,sprintf('<g data-node-id="%s"><title>%s</title><rect x="%s" y="%s" width="92" height="36" rx="7" fill="#f4f7fa" stroke="#34546d"/><text x="%s" y="%s" text-anchor="middle" font-family="sans-serif" font-size="12">%s</text></g>',pl_html_escape(i),label,p[1]-46,p[2]-18,p[1],p[2]+4,label))
    }
    paste(c(svg,sprintf('<text x="12" y="%s" font-family="sans-serif" font-size="11" fill="#704628">Dashed lines indicate moderation of a selected arrow.</text></svg>',layout$height-4)),collapse="\n")
}
