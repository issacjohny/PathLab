pathsClass <- if(requireNamespace("jmvcore",quietly=TRUE)) R6::R6Class("pathsClass",inherit=pathsBase,public=list(
    optionsChangedHandler=function(optionNames) {
        if(any(!pl_native_is_presentation(optionNames))) {
            self$results$fitCache$setState(NULL);self$results$fitCache$setContent("")
            private$.allowSavedCache<-FALSE
            private$.savedFitText<-""
        }
        private$.clearOutputs("Updating results for the current selections.")
        super$optionsChangedHandler(optionNames)
    }
),private=list(
    .allowSavedCache=TRUE,
    .savedFitText="",
    # Dynamic rows are not managed by a YAML rows expression. Always replace them
    # on a rerun, including when the next specification is incomplete or fails.
    .clearOutputs=function(message="") {
        for(name in self$results$itemNames) {
            if(name=="fitCache")next
            item<-self$results[[name]]
            if(inherits(item,"Table")) item$deleteRows()
            else if(inherits(item,"Array")) {item$clear();if(name=="interactionPlots")item$setVisible(FALSE)}
            else if(inherits(item,c("Preformatted","Html"))) item$setContent("")
            else if(inherits(item,"Image")) { item$setState(NULL);item$setVisible(FALSE) }
            # jamovi's Html renderer retains its previous DOM for empty content.
            # Explicit visibility also removes that stale image during reruns.
            if(name %in% c("diagram","coding","logisticFit"))item$setVisible(FALSE)
        }
        self$results$setStatus("none")
        self$results$review$setContent(message)
    },
    .init=function() {
        # Desktop 28 saves this initialized result tree before calling .run.
        # Capture data-only text now, before that save replaces the old file.
        # Its fingerprint and payload are validated after the full data arrive.
        if(isTRUE(private$.allowSavedCache)&&!nzchar(private$.savedFitText))
            private$.savedFitText<-pl_fit_cache_saved_text(private$.statePathSource())
        # Init and run can use different R instances. Carry the data-only text
        # through the host's intervening .save, rather than only a private field.
        if(nzchar(private$.savedFitText))self$results$fitCache$setContent(private$.savedFitText)
        private$.clearOutputs("Select X and Y, choose continuous or binary logistic outcome, and specify every intended arrow. Categorical predictors, moderators and covariates need an explicit coding row and reference. Mediators remain continuous.")
    },
    .clear=function(vChanges) {
        if(length(vChanges)) {
            self$results$fitCache$setState(NULL);self$results$fitCache$setContent("")
            private$.allowSavedCache<-FALSE
            private$.savedFitText<-""
        }
        private$.clearOutputs("PathLab is restoring the saved specification and recalculating its results.")
    },
    .run=function() {
        private$.clearOutputs()
        o<-self$options
        if(any(vapply(list(o$x,o$y),function(v)is.null(v)||!length(v),TRUE))) {
            self$results$fitCache$setState(NULL)
            self$results$fitCache$setContent("")
            private$.savedFitText<-""
            self$results$review$setContent("Select X and Y to fit the analysis. Specify arrows, interactions, categorical references and outcome family. No model has been fitted.")
            return()
        }
        tryCatch(private$.fit(),error=function(e) {
            # Preserve the engine's cooperative restart protocol. A superseded
            # analysis is not a model failure and must publish no partial fit.
            if(identical(e$code,"restart")) {
                private$.clearOutputs("The previous calculation was cancelled. PathLab will use the current specification.")
                stop(e)
            }
            self$results$fitCache$setState(NULL)
            self$results$fitCache$setContent("")
            private$.savedFitText<-""
            private$.clearOutputs(paste("PathLab could not fit the current specification.",conditionMessage(e),"Review the selected variables, arrows and settings."))
            stop(e)
        })
    },
    .fit=function() {
        o<-self$options
        checkpoint<-function()private$.checkpoint(flush=FALSE)
        checkpoint()
        pl_require(is.null(attr(self$data,"jmv-weights",exact=TRUE)),"UNSUPPORTED_FEATURE","PathLab does not support active jamovi weights. Disable weights before running this analysis.")
        fields<-pl_native_model_fields()
        controls<-setNames(lapply(fields,function(i)o[[i]]),fields)
        spec<-native_spec(controls)
        settings<-native_settings(controls,spec,self$data)
        prepared<-pl_native_prepare_data(self$data,spec,controls);spec<-prepared$spec
        estimation<-list(draws=o$draws,seed=o$seed,robust=o$robust,confidence=o$confidence)
        key<-pl_fit_cache_key(self$data,controls,estimation)
        entry<-self$results$fitCache$state
        cache<-pl_fit_cache_read(entry,key)
        if(is.null(entry)&&isTRUE(private$.allowSavedCache)) {
            cache<-pl_fit_cache_from_json(self$results$fitCache$content,key)
            if(is.null(cache))cache<-pl_fit_cache_from_json(private$.savedFitText,key)
            if(is.null(cache))cache<-pl_fit_cache_saved(private$.statePathSource(),key)
        }
        reused<-!is.null(cache)
        if(!reused) {
            self$results$fitCache$setState(NULL)
            r<-private$.computeFit(prepared$data,spec,settings)
            r$binary_coding<-prepared$binary
            # jamovi preserves original numeric row names after filtering. Keep
            # that mapping alongside positions in the delivered frame. Arbitrary
            # text row labels may identify people and are never serialized.
            row_numbers<-suppressWarnings(as.integer(row.names(self$data)))
            if(length(row_numbers)==nrow(self$data) && !anyNA(row_numbers) &&
                all(row_numbers>0L) && !anyDuplicated(row_numbers)) {
                r$sample$delivered_row_numbers<-row_numbers
                r$sample$included_row_numbers<-row_numbers[r$sample$included_rows]
            }
            cache<-pl_fit_cache_entry(key,r)
        }
        self$results$fitCache$setState(cache)
        self$results$fitCache$setContent(pl_fit_cache_json(cache))
        private$.savedFitText<-""
        r<-cache$result
        r$fit_cache<-list(schema=cache$schema,status=if(reused)"reused" else "computed",
            fit_id=cache$fit_id,input_sha256=key,computed_utc=cache$computed_utc,
            compressed_summary_bytes=cache$compressed_bytes)
        plot_fields<-setdiff(pl_native_presentation_fields(),c("showDetails","showCovariance"))
        plots<-pl_plot_states(r,prepared$data,setNames(lapply(plot_fields,function(n)o[[n]]),plot_fields))
        checkpoint()
        for(name in c("effectPlot","interactionPlot","jnPlot"))if(!is.null(plots[[name]])) {
            self$results[[name]]$setState(plots[[name]])
            if(name=="effectPlot")self$results[[name]]$setSize(760L,max(560L,as.integer(160+42*length(plots[[name]]$rows))))
            self$results[[name]]$setVisible(TRUE)
        }
        if(length(plots$interactionPlots)) {
            self$results$interactionPlots$setVisible(TRUE)
            for(key in names(plots$interactionPlots)) {
                checkpoint()
                image<-self$results$interactionPlots$addItem(key)
                image$setTitle(plots$interactionPlots[[key]]$title)
                image$setState(plots$interactionPlots[[key]])
            }
        }
        self$results$plotNotes$setContent(paste(plots$notes,collapse="\n"))
        # Aggregate coordinates are auditable without serializing respondent data.
        r$plots<-plots
        r$plot_conditions<-prepare_sample(prepared$data,validate_spec(spec,settings,names(prepared$data)))$conditions
        logistic<-settings$outcome_family=="logistic"
        condition<-function(c)pl_condition_label(spec,c)
        edges<-vapply(spec$edges,function(e)pl_path_label(spec,e$id),"")
        mods<-vapply(spec$moderations,function(m)paste(paste(vapply(unlist(m$moderator_ids),function(id)pl_node_label(spec,id),""),collapse=" jointly with "),"moderates",pl_path_label(spec,m$path_id)),"")
        review<-c(paste("Development preview:",if(logistic)"continuous mediators and binary logistic outcome" else "linear observed-variable graph"),edges,mods,
          paste("N =",r$sample$n,"of",r$sample$source_n,"rows delivered by jamovi; excluded =",length(r$sample$exclusions)),
          "jamovi filters and user-defined missing rules are applied before analysis. Included rows refer to the delivered frame; numeric delivered/included row numbers are recorded when available.",
          "Common complete-case sample; independent observations assumed. No centering or score construction.",
          paste("Covariance:",if(o$robust)"HC3 (generalized HC3 for logistic); normal reference" else if(logistic)"classical OLS t; model-based logistic normal z" else "classical; t reference"),
          paste("Percentile bootstrap;",o$confidence,"% pointwise intervals;",r$bootstrap$successful,"/",r$bootstrap$requested,"successful; seed",o$seed),
          paste("Fixed probes:",paste(names(r$sample$probes),vapply(r$sample$probes,paste,"",collapse=", "),collapse="; ")),
          if(length(spec$moderations))"Path slopes and Johnson-Neyman slices hold other numerical predictors at complete-case means and categorical predictors at their declared reference.",
          if(length(r$coding))vapply(r$coding,function(row)paste("Indicator coding:",row$column,"levels",paste(vapply(as.list(row$levels),pl_value_label,""),collapse=", "),"; reference",pl_value_label(row$reference)),""),
          if(logistic)paste("Binary event:",prepared$binary$event,"= 1;",prepared$binary$non_event,"= 0. Logistic MLE converged; original-sample separation checked."),
          if(logistic)"Indirect and summed associations are log-odds statistical path products. They are not probability effects, natural causal effects, odds ratios of indirect effects or a reduced-model total coefficient.",
          if(r$bootstrap$failed>0)"Some requested resamples failed. No draws were replaced; review recorded failure reasons and the 95% success threshold.",
          if(length(r$sample$extrapolation))paste("Extrapolation:",paste(r$sample$extrapolation,collapse=", ")),
          "Path products are statistical associations, not causal effects. No global SEM fit is claimed.")
        self$results$review$setContent(paste(review,collapse="\n"))
        self$results$diagram$setContent(pl_diagram(spec))
        self$results$diagram$setVisible(TRUE)
        for(target in names(r$equations)) {
            checkpoint()
            e<-r$equations[[target]]; f<-e$fit
            self$results$equations$addRow(rowKey=target,values=list(target=pl_node_label(spec,target),equation=e$equation,n=r$sample$n,df=f$df_residual,r2=if(is.null(f$r_squared))NA_real_ else f$r_squared,rmse=if(is.null(f$rmse))NA_real_ else f$rmse,test=f$model_test$reference,statistic=f$model_test$statistic,p=f$model_test$p))
            for(j in seq_along(e$terms)) {
                term<-names(e$terms)[j]
                self$results$estimates$addRow(rowKey=paste(target,j),values=list(target=pl_node_label(spec,target),term=pl_term_label(spec,term,e$basis_description),estimate=f$coefficients[j],se=f$se[j],statistic=f$statistic[j],p=f$p[j],lower=f$lower[j],upper=f$upper[j]))
                for(k in seq_along(e$terms)) self$results$covariance$addRow(rowKey=paste(target,j,k),values=list(target=pl_node_label(spec,target),term1=pl_term_label(spec,term,e$basis_description),term2=pl_term_label(spec,names(e$terms)[k],e$basis_description),value=f$covariance[j,k]))
            }
            if(identical(e$family,"logistic"))self$results$logisticFit$addRow(rowKey=target,values=list(target=pl_node_label(spec,target),event=as.character(prepared$binary$event),nonEvent=as.character(prepared$binary$non_event),ll=f$log_likelihood,nullLL=f$null_log_likelihood,pseudoR2=f$pseudo_R2,aic=f$AIC,bic=f$BIC,converged=if(isTRUE(f$convergence$converged))"Yes" else "No",iterations=f$convergence$iterations))
        }
        self$results$coding$setVisible(length(r$coding)>0);self$results$logisticFit$setVisible(logistic)
        for(row in r$coding)for(j in seq_along(row$counts)) {
            count<-row$counts[[j]]
            self$results$coding$addRow(rowKey=paste(row$node_id,j),values=list(role=toupper(sub("^cov","C",row$node_id)),column=row$column,level=pl_value_label(count$level),reference=pl_value_label(row$reference),n=count$n,basis=if(count$level==row$reference)"Reference (all indicators 0)" else paste(pl_value_label(count$level),"vs",pl_value_label(row$reference))))
        }
        self$results$effects$setTitle(paste0("Conditional effects, indices and paired contrasts",if(logistic)" (log-odds path products)" else ""))
        if(identical(o$modelMode,"general"))pl_populate_effect_pages(self$results$effectPages,spec,r$estimands,if(logistic)"log-odds path products" else NULL,checkpoint=checkpoint) else {
            lookup<-setNames(r$estimands,vapply(r$estimands,`[[`,"","id"))
            for(i in seq_along(r$estimands)) {
                if(i%%50L==1L)checkpoint()
                row<-r$estimands[[i]]
                self$results$effects$addRow(rowKey=row$id,values=pl_effect_values(spec,row,lookup))
            }
        }
        for(j in seq_along(r$slopes)) {
            if(j%%50L==1L)checkpoint()
            row<-r$slopes[[j]]
            label<-condition(row$condition)
            if(length(row$held_at_means))label<-paste(label,"; at means:",condition(row$held_at_means))
            if(length(row$held_at_references))label<-paste(label,"; at references:",condition(row$held_at_references))
            if(identical(row$scale,"log_odds"))label<-paste(label,"; log-odds coefficient scale")
            self$results$simpleSlopes$addRow(rowKey=j,values=list(path=pl_focal_label(pl_path_label(spec,row$path_id),row$focal_contrast),condition=label,estimate=row$estimate,se=row$se,p=row$p,lower=row$lower,upper=row$upper))
        }
        for(j in seq_along(r$johnson_neyman)) {
            if(j%%50L==1L)checkpoint()
            row<-r$johnson_neyman[[j]]; bounds<-row$roots
            if(length(bounds))bounds<-bounds[bounds>=row$observed_range[1] & bounds<=row$observed_range[2]]
            note<-if(row$status=="available")"Pointwise path-slope inference; not an indirect-effect region" else row$reason
            if(length(row$other_conditions))note<-paste(note,"; fixed",condition(row$other_conditions))
            if(length(row$conditioning_note))note<-paste(note,row$conditioning_note,sep="; ")
            self$results$jn$addRow(rowKey=j,values=list(path=pl_focal_label(pl_path_label(spec,row$path_id),row$focal_contrast),moderator=pl_node_label(spec,row$moderator_id),boundaries=if(row$status!="available")"Not applicable" else if(length(bounds))paste(formatC(bounds,digits=5,format="fg"),collapse=", ") else "No in-range boundary",range=if(length(row$observed_range))paste(formatC(row$observed_range,digits=5,format="fg"),collapse=" to ") else "",note=note))
        }
        self$results$diagnostics$setContent(jsonlite::toJSON(r[c("slopes","johnson_neyman","interaction_tests","residual_correlations","bootstrap")],auto_unbox=TRUE,pretty=TRUE,digits=16))
        self$results$reproduction$setContent(jsonlite::toJSON(pl_public_result(r),auto_unbox=TRUE,pretty=TRUE,digits=16,null="null"))
    },
    .computeFit=function(data,spec,settings) {
        o<-self$options
        last<- -Inf
        progress<-function(completed,total) {
            now<-unname(proc.time()[[3L]])
            flush<-completed==0L || completed==total || now-last>=.5
            if(flush) {
                self$results$review$setContent(paste("Computing paired bootstrap:",completed,"of",total,"resamples. Changing the model or inference settings cancels this calculation and starts the current specification."))
                last<<-now
            }
            private$.checkpoint(flush=flush)
        }
        pathlab_fit(data,spec,settings,draws=o$draws,seed=o$seed,robust=o$robust,alpha=1-o$confidence/100,progress=progress)
    },
    .renderPlot=function(image,...)pl_render_plot(image$state)
))
