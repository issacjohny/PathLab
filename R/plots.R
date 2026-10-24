# Native Image states contain aggregate coordinates only, never respondent rows,
# fitted objects or bootstrap arrays. Rendering does not refit the analysis.
pl_plot_role <- function(role) {
    role<-tolower(role)
    if(grepl("^c[0-9]+$",role))sub("^c","cov",role) else role
}
pl_plot_profile_matrix <- function(equation,source,values,condition) {
    n<-length(values)
    at<-function(factor) {
        d<-equation$basis_description[[factor]];id<-d$node_id
        v<-if(id==source)values else condition[[id]]
        if(is.null(v))v<-if(is.null(d$level))equation$means[[id]] else equation$coding[[id]]$reference
        if(!is.null(d$level))v<-as.numeric(v==d$level)
        rep_len(as.numeric(v),n)
    }
    vapply(equation$terms,function(term)if(length(term))Reduce(`*`,lapply(term,at)) else rep(1,n),numeric(n))
}
pl_plot_interval <- function(equation,matrix,alpha) {
    f<-equation$fit;estimate<-as.numeric(matrix%*%f$coefficients)
    variance<-rowSums((matrix%*%f$covariance)*matrix)
    tolerance<-128*.Machine$double.eps*pmax(1,rowSums(abs((matrix%*%f$covariance)*matrix)))
    pl_require(all(is.finite(estimate)) && all(is.finite(variance)) && all(variance>= -tolerance),"NUMERICAL_FAILURE","Plot intervals could not be evaluated finitely")
    se<-sqrt(pmax(0,variance));critical<-if(f$reference=="t")stats::qt(1-alpha/2,f$df_residual) else stats::qnorm(1-alpha/2)
    list(estimate=estimate,se=se,lower=estimate-critical*se,upper=estimate+critical*se)
}
pl_plot_states <- function(result,data,options) {
    get<-function(name,default)if(is.null(options[[name]]))default else options[[name]]
    out<-list(effectPlot=NULL,interactionPlot=NULL,jnPlot=NULL,notes=character())
    enabled<-vapply(c("showEffectPlot","showInteractionPlot","showJNPlot"),function(n)isTRUE(get(n,FALSE)),TRUE)
    if(!any(enabled))return(out)
    spec<-result$graph;alpha<-result$provenance$alpha
    confidence<-paste0(format(100*(1-alpha),trim=TRUE),"%")
    logistic<-identical(result$requested_settings$outcome_family,"logistic")
    if(enabled[2] && identical(get("plotInteractionMode","selected"),"all")) {
        pairs<-list();seen<-character()
        for(m in spec$moderations)for(w in unlist(m$moderator_ids)) {
            key<-paste(m$path_id,w,sep="|")
            if(!key %in% seen) {
                edge<-Filter(function(e)e$id==m$path_id,spec$edges)[[1]]
                pairs[[length(pairs)+1L]]<-list(source=edge$source,target=edge$target,moderator=w)
                seen<-c(seen,key)
            }
        }
        page<-get("plotInteractionPage",1L);pages<-ceiling(length(pairs)/6)
        out$interactionPlots<-list()
        out$interactionPagination<-list(total=length(pairs),pages=pages,page=page,per_page=6L)
        if(!length(pairs))out$notes<-c(out$notes,"All interactions: no declared moderated arrows.")
        else if(page<1 || page>pages || page!=floor(page))out$notes<-c(out$notes,paste("All interactions: choose a page from 1 to",pages))
        else {
            indices<-seq.int((page-1L)*6L+1L,min(page*6L,length(pairs)))
            for(i in indices) {
                pair<-pairs[[i]];selected<-options
                selected$plotInteractionMode<-"selected";selected$showEffectPlot<-FALSE;selected$showJNPlot<-FALSE
                selected$plotSource<-pair$source;selected$plotTarget<-pair$target;selected$plotModerator<-pair$moderator
                plotted<-pl_plot_states(result,data,selected)
                state<-plotted$interactionPlot
                if(!is.null(state)) {
                    state$title<-paste(state$title,"|",pl_node_label(spec,pair$moderator))
                    out$interactionPlots[[as.character(i)]]<-state
                }
                out$notes<-c(out$notes,plotted$notes)
            }
            out$notes<-c(paste("All declared interactions:",length(pairs),"arrow/moderator slices across",pages,"pages; page",page,"(six per page). Other moderators use the selected fixed probe combination. Joint interactions are conditional slices, not a single three-dimensional surface."),out$notes)
        }
        enabled[2]<-FALSE
    }
    if(enabled[1]) {
        kind<-get("plotEffectKind","specific_indirect");page<-get("plotEffectPage",1L)
        choose<-function(r)switch(kind,indices=r$kind %in% c("linear_index","mixed_index","conditional_index","conditional_derivative","conditional_mixed_derivative"),contrasts=r$kind %in% c("route_difference","condition_difference","route_derivative_difference"),identical(r$kind,kind))
        rows<-Filter(choose,result$estimands);pages<-ceiling(length(rows)/12)
        if(!length(rows))out$notes<-c(out$notes,"Effect plot: no estimands of the selected kind are applicable.")
        else if(page<1 || page>pages || page!=floor(page))out$notes<-c(out$notes,paste("Effect plot: select a page from 1 to",pages,"for",length(rows),"estimands."))
        else {
            ii<-seq.int((page-1L)*12L+1L,min(page*12L,length(rows)));lookup<-setNames(result$estimands,vapply(result$estimands,`[[`,"","id"))
            selected<-lapply(rows[ii],function(r)list(id=r$id,kind=r$kind,label=pl_estimand_label(spec,r,lookup=lookup),condition=paste(pl_condition_label(spec,r$condition),if(length(r$condition_b))paste("minus",pl_condition_label(spec,r$condition_b))),estimate=r$estimate,lower=r$lower,upper=r$upper))
            out$effectPlot<-list(type="effect",title=paste("Conditional associations \u2014 page",page,"of",pages),rows=selected,page=page,pages=pages,total=length(rows),kind=kind,x_label=if(logistic)"Log-odds statistical path product" else "Statistical path association",caption=paste(confidence,"pointwise percentile bootstrap intervals; no multiplicity adjustment."))
            out$notes<-c(out$notes,paste("Effect plot:",length(rows),"selected estimands across",pages,"pages (12 per page). Change Effect page to view every row."))
        }
    }
    if(!any(enabled[2:3]))return(out)
    model<-validate_spec(spec,result$requested_settings,names(data));sample<-prepare_sample(data,model)
    source<-pl_plot_role(get("plotSource","X"));target<-pl_plot_role(get("plotTarget","Y"));moderator<-pl_plot_role(get("plotModerator","W"))
    found<-Filter(function(e)e$source==source && e$target==target,spec$edges)
    if(!length(found)) { out$notes<-c(out$notes,"Interaction / JN plots: the selected arrow is absent. Choose an explicitly specified arrow.");return(out) }
    edge<-found[[1]];eq<-result$equations[[target]]
    attached<-unique(unlist(lapply(Filter(function(m)m$path_id==edge$id,spec$moderations),`[[`,"moderator_ids")))
    if(!(moderator %in% attached)) { out$notes<-c(out$notes,"Interaction / JN plots: the selected moderator is not attached to that arrow.");return(out) }
    ci<-get("plotCondition",1L)
    if(ci<1 || ci>length(sample$conditions) || ci!=floor(ci)) { out$notes<-c(out$notes,paste("Interaction / JN plots: select a probe-combination number from 1 to",length(sample$conditions)));return(out) }
    condition<-sample$conditions[[ci]];other<-condition[setdiff(names(condition),c(source,moderator))]
    held_means<-eq$means[setdiff(names(eq$means),c(source,moderator,names(other),target))]
    predictors<-unique(vapply(eq$basis_description[unique(unlist(eq$terms))],`[[`,"","node_id"))
    held_means<-held_means[intersect(names(held_means),predictors)]
    held_references<-lapply(eq$coding[intersect(setdiff(names(eq$coding),c(source,moderator,names(other))),predictors)],`[[`,"reference")
    conditioning<-paste("Fixed probes:",pl_condition_label(spec,other),"; other predictors at complete-case means / declared references.")
    path<-pl_path_label(spec,edge$id)
    if(enabled[2]) {
        categorical<-source %in% names(model$coding)
        values<-if(categorical)unlist(model$coding[[source]]$levels) else seq(min(sample$values[[source]]),max(sample$values[[source]]),length.out=101L)
        series<-lapply(as.list(sample$probes[[moderator]]),function(value) {
            profile<-other;profile[[moderator]]<-value
            interval<-pl_plot_interval(eq,pl_plot_profile_matrix(eq,source,values,profile),alpha)
            response<-identical(eq$family,"logistic") && identical(get("plotResponseScale","response"),"response")
            if(response)for(name in c("estimate","lower","upper"))interval[[name]]<-stats::plogis(interval[[name]])
            c(list(label=pl_condition_label(spec,setNames(list(value),moderator)),moderator_value=value),interval)
        })
        response<-identical(eq$family,"logistic") && identical(get("plotResponseScale","response"),"response")
        event<-result$binary_coding$event
        out$interactionPlot<-list(type="interaction",title=paste("Fitted interaction:",path),path_id=edge$id,moderator_id=moderator,condition=other,held_at_means=held_means,held_at_references=held_references,x=values,categorical=categorical,series=series,x_label=pl_node_label(spec,source),y_label=if(response)paste0("Pr(",pl_node_label(spec,target)," = ",if(is.null(event))"1" else event,")") else if(identical(eq$family,"logistic"))"Fitted log odds" else paste("Fitted",pl_node_label(spec,target)),scale=if(response)"probability" else if(identical(eq$family,"logistic"))"log_odds" else "outcome_units",caption=paste(confidence,"pointwise fitted-mean intervals; not prediction intervals."),conditioning=conditioning)
        out$notes<-c(out$notes,paste("Interaction:",path,";",conditioning,"Lines use fixed moderator probes; bands are analytic, not bootstrap intervals. Logistic curves are conditional equation predictions, not indirect effects."))
    }
    if(enabled[3]) {
        contrast<-get("plotContrast",1L);factors<-sample$basis[[source]]
        if(contrast<1 || contrast>length(factors) || contrast!=floor(contrast)) { out$notes<-c(out$notes,paste("JN plot: select focal contrast from 1 to",length(factors)));return(out) }
        factor<-factors[contrast];focal<-if(source %in% names(model$coding))sample$basis_description[[factor]] else NULL
        same<-function(a,b)isTRUE(all.equal(a,b,check.attributes=FALSE))
        matches<-Filter(function(r)identical(r$path_id,edge$id) && identical(r$moderator_id,moderator) && same(r$focal_contrast,focal) && same(r$other_conditions,other),result$johnson_neyman)
        if(!length(matches)) { out$notes<-c(out$notes,"JN plot: no applicable slice matches the selected arrow and probe combination.");return(out) }
        row<-matches[[1]]
        if(row$status!="available") { out$notes<-c(out$notes,paste("JN plot:",row$reason));return(out) }
        x<-sort(unique(c(seq(row$observed_range[1],row$observed_range[2],length.out=201L),row$boundaries)))
        matrix<-t(vapply(x,function(value) { profile<-other;profile[[moderator]]<-value;pl_slope_vector(edge,eq,profile,focal_factor=factor) },numeric(length(eq$terms))))
        interval<-pl_plot_interval(eq,matrix,alpha)
        out$jnPlot<-c(list(type="jn",title=paste("Johnson\u2013Neyman:",pl_focal_label(path,focal)),path_id=edge$id,moderator_id=moderator,focal_contrast=focal,condition=other,held_at_means=row$held_at_means,held_at_references=row$held_at_references,x=x,roots=row$roots,boundaries=row$boundaries,regions=row$regions,observed_range=row$observed_range,x_label=pl_node_label(spec,moderator),y_label=if(identical(eq$family,"logistic"))"Conditional path slope (log odds)" else "Conditional path slope",caption=paste(confidence,"pointwise analytic intervals; shaded regions exclude zero."),conditioning=conditioning),interval)
        out$notes<-c(out$notes,paste("Johnson\u2013Neyman:",pl_focal_label(path,focal),";",conditioning,"Observed moderator range only. This is a path-slope region, not an indirect-effect region or a simultaneous confidence band."))
    }
    out
}

pl_plot_wrap <- function(text,width=95)paste(strwrap(text,width=width),collapse="\n")
pl_render_plot <- function(state) {
    if(is.null(state))return(FALSE)
    # Restore only styling changed here. On tiny devices the initial computed
    # pin/plt can be invalid; restoring those derived values itself raises.
    old<-graphics::par(c("mar","mgp","cex","bg","fg","col.axis","family"));on.exit(graphics::par(old),add=TRUE)
    # Keep ordinary-size labels fixed, but let a small resized/exported canvas
    # render without base graphics' "figure margins too large" failure.
    compact<-min(1,graphics::par("din")/c(3.5,2.6))
    graphics::par(mar=c(6,5,5,1)*compact,mgp=c(2.8,.7,0)*compact,cex=compact,bg="white",fg="#223344",col.axis="#334455",family="sans")
    palette<-c("#17679A","#A74322","#287346","#77539C","#8C6500")
    if(state$type=="effect") {
        rows<-state$rows;n<-length(rows);y<-rev(seq_len(n));range<-range(c(0,unlist(lapply(rows,function(r)c(r$lower,r$upper)))))
        graphics::par(mar=c(5,1,4,1)*compact)
        graphics::plot(range,c(.5,n+.5),type="n",yaxt="n",xlab=state$x_label,ylab="",main=state$title)
        graphics::abline(v=0,lty=2,col="#8B9299")
        # Labels sit above each interval within the plotting area, avoiding a
        # fixed-width margin that would clip long categorical/route identifiers.
        for(i in seq_len(n)) {
            r<-rows[[i]]
            graphics::segments(r$lower,y[i]-.16,r$upper,y[i]-.16,col=palette[1],lwd=2)
            graphics::points(r$estimate,y[i]-.16,pch=19,col=palette[1])
            label<-paste(r$label,"|",r$condition)
            graphics::text(range[1],y[i]+.16,pl_plot_wrap(label,100),adj=c(0,0),cex=.82)
        }
        graphics::mtext(state$caption,side=1,line=3.6,cex=.82)
        return(TRUE)
    }
    yvalues<-if(state$type=="jn")c(state$lower,state$upper,0) else unlist(lapply(state$series,function(s)c(s$lower,s$upper)))
    x<-if(isTRUE(state$categorical))seq_along(state$x) else state$x
    graphics::plot(range(x),range(yvalues),type="n",xlab=state$x_label,ylab=state$y_label,main=pl_plot_wrap(state$title,72),xaxt=if(isTRUE(state$categorical))"n" else "s")
    if(isTRUE(state$categorical))graphics::axis(1,at=x,labels=vapply(as.list(state$x),pl_value_label,""))
    if(state$type=="jn") {
        bounds<-graphics::par("usr")
        for(region in state$regions)if(isTRUE(region$significant))graphics::rect(region$lower,bounds[3],region$upper,bounds[4],col="#E4F2E8",border=NA)
        graphics::polygon(c(x,rev(x)),c(state$lower,rev(state$upper)),col=grDevices::adjustcolor(palette[1],alpha.f=.16),border=NA)
        graphics::lines(x,state$lower,col=palette[1],lty=3);graphics::lines(x,state$upper,col=palette[1],lty=3)
        graphics::lines(x,state$estimate,col=palette[1],lwd=2)
        graphics::abline(h=0,lty=2,col="#444444")
        if(length(state$boundaries))graphics::abline(v=state$boundaries,lty=2,col="#A74322")
        label<-if(length(state$boundaries))paste("JN boundaries:",paste(formatC(state$boundaries,digits=5,format="fg"),collapse=", ")) else "No Johnson\u2013Neyman boundary in the observed range."
        graphics::mtext(label,side=1,line=4.8,cex=.8)
    } else {
        if(!isTRUE(state$categorical))for(k in seq_along(state$series)) {
            s<-state$series[[k]]
            graphics::polygon(c(x,rev(x)),c(s$lower,rev(s$upper)),col=grDevices::adjustcolor(palette[k],alpha.f=.12),border=NA)
        }
        for(k in seq_along(state$series)) {
            s<-state$series[[k]];color<-palette[k]
            if(isTRUE(state$categorical)) {
                shifted<-x+(k-(length(state$series)+1)/2)*.09
                graphics::segments(shifted,s$lower,shifted,s$upper,col=color,lwd=1.5)
                graphics::points(shifted,s$estimate,col=color,pch=14+k,cex=1.1)
            } else {
                graphics::lines(x,s$estimate,col=color,lwd=2,lty=k)
            }
        }
        # legend() tests whether any line type is positive. An all-NA lty
        # makes that decision NA and fails in the Desktop PNG renderer.
        graphics::legend("topleft",legend=vapply(state$series,`[[`,"","label"),col=palette[seq_along(state$series)],lty=if(isTRUE(state$categorical))0 else seq_along(state$series),pch=if(isTRUE(state$categorical))14+seq_along(state$series) else NA,bg="white",cex=.85)
    }
    graphics::mtext(pl_plot_wrap(state$conditioning,100),side=3,line=.3,cex=.72)
    graphics::mtext(state$caption,side=1,line=3.6,cex=.8)
    TRUE
}
