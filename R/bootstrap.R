pl_check_work <- function(model,sample,equations,draws) {
    route_count<-sum(vapply(model$routes,function(r)length(r$path_ids)>1,TRUE))
    conditions<-length(sample$conditions); moderator_count<-length(model$numerical_moderators)
    focal_count<-if(length(sample$basis))length(sample$basis[[model$x]]) else 1L
    estimated<-focal_count*(conditions*(route_count+3)+conditions*choose(route_count,2)+(route_count+3)*choose(conditions,2)+conditions*(route_count+1+choose(route_count,2))*(2^moderator_count-1))
    pl_require(estimated<=8000,"RESOURCE_LIMIT","Requested effects and contrasts exceed the result budget")
    p<-vapply(equations,function(e)ncol(e$design),0L)
    multipliers<-vapply(equations,function(e)if(identical(e$family,"logistic"))10 else 1,0.0)
    pl_require(sample$n*sum(p^2*multipliers)*draws<=2e9 && sum(vapply(equations,function(e)length(e$design),0L))<=5e6,"RESOURCE_LIMIT")
    routecost<-sum(vapply(model$routes,function(r)sum(vapply(seq_len(moderator_count),function(k)choose(moderator_count,k)*length(r$path_ids)^(k+1),0.0)),0.0))
    pl_require(draws*length(sample$conditions)*focal_count*routecost<=1e8,"RESOURCE_LIMIT")
    invisible(TRUE)
}

#' Paired whole-case bootstrap with fixed original-sample probes
#' @export
bootstrap_estimands <- function(model,sample,equations,draws=1000L,seed=419L,indices=NULL,alpha=.05,progress=NULL) {
    pl_require(is.null(progress)||is.function(progress),"INVALID_OPTIONS","progress must be NULL or a callback function")
    if(!is.null(progress))progress(0L,as.integer(draws))
    pl_check_work(model,sample,equations,draws)
    original<-evaluate_estimands(model,sample,equations)
    if(is.null(indices)) {
        # Restore both RNG kind and state even after an error.
        kind<-RNGkind(); existed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
        if(existed) previous<-get(".Random.seed",envir=.GlobalEnv)
        on.exit({ do.call(RNGkind,as.list(kind)); if(existed)assign(".Random.seed",previous,envir=.GlobalEnv) else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv) },add=TRUE)
        RNGkind("Mersenne-Twister","Inversion","Rejection"); set.seed(seed)
        indices<-matrix(sample.int(sample$n,sample$n*draws,replace=TRUE),nrow=draws,byrow=TRUE)
        rng<-"R Mersenne-Twister/Inversion/Rejection"
    } else rng<-"supplied shared row-index matrix (1-based)"
    pl_require(is.matrix(indices) && identical(dim(indices),c(as.integer(draws),as.integer(sample$n))) && is.numeric(indices) && all(is.finite(indices)) && all(indices==floor(indices)) && all(indices>=1 & indices<=sample$n),"INVALID_OPTIONS","Resample matrix must have draws rows and complete-case N columns")
    plan<-pl_compile_estimand_plan(model,sample,equations,original)
    distribution<-matrix(NA_real_,draws,length(original)); failures<-character()
    for(b in seq_len(draws)) {
        # Outside the draw-error handler: cancellation/restart is not a failed
        # resample and must unwind immediately, restoring RNG via on.exit.
        if(!is.null(progress) && b%%10L==0L)progress(b-1L,as.integer(draws))
        ans<-tryCatch({
            betas<-lapply(equations,function(e)pl_fit(e$design[indices[b,],,drop=FALSE],e$y[indices[b,]],coefficients_only=TRUE,family=if(is.null(e$family))"linear" else e$family))
            pl_evaluate_estimand_plan(plan,betas)
        },pathlab_error=function(e)e,error=function(e)structure(list(code="NUMERICAL_FAILURE",message=conditionMessage(e)),class=c("pathlab_error","error","condition")))
        if(inherits(ans,"pathlab_error")) failures<-c(failures,ans$code) else {
            pl_require(all(is.finite(ans)),"NUMERICAL_FAILURE"); distribution[b,]<-ans
        }
    }
    if(!is.null(progress))progress(as.integer(draws),as.integer(draws))
    success<-rowSums(is.na(distribution))==0
    pl_require(sum(success)>=.95*draws,"BOOTSTRAP_FAILURE",paste(sum(success),"of",draws,"draws succeeded;",paste(names(table(failures)),table(failures),collapse=", ")))
    distribution<-distribution[success,,drop=FALSE]
    rows<-lapply(seq_along(original),function(i)c(original[[i]],list(bootstrap_se=sd(distribution[,i]),lower=unname(quantile(distribution[,i],alpha/2,type=7)),upper=unname(quantile(distribution[,i],1-alpha/2,type=7)))))
    list(rows=rows,distribution=distribution,accepted_draws=which(success),metadata=list(requested=draws,successful=sum(success),failed=sum(!success),failure_reasons=as.list(table(failures)),seed=seed,rng=rng,ci_method="percentile",confidence_level=1-alpha,quantile_method="R type 7 / NumPy linear",sd_ddof=1,success_threshold=.95))
}

#' Fit an explicitly specified observed-variable conditional-process graph
#' @export
pathlab_fit <- function(data,spec,settings=list(outcome_family="linear",categorical=list(),probes=list()),draws=1000L,seed=419L,robust=TRUE,alpha=.05,indices=NULL,progress=NULL) {
    pl_require(length(draws)==1 && is.finite(draws) && draws==as.integer(draws) && draws>=200 && draws<=5000,"INVALID_OPTIONS")
    pl_require(length(seed)==1 && is.finite(seed) && seed==as.integer(seed) && seed>=0,"INVALID_OPTIONS")
    pl_require(is.logical(robust) && length(robust)==1 && !is.na(robust) && length(alpha)==1 && is.finite(alpha) && alpha>0 && alpha<.5,"INVALID_OPTIONS")
    model<-validate_spec(spec,settings,names(data)); sample<-prepare_sample(data,model)
    compiled<-compile_equations(model,sample)
    pl_check_work(model,sample,compiled,draws)
    equations<-fit_equations(compiled,robust,alpha)
    boot<-bootstrap_estimands(model,sample,equations,as.integer(draws),as.integer(seed),indices,alpha,progress)
    diagnostics<-pl_slopes(model,sample,equations,alpha)
    sourcefile<-system.file("provenance","build-source.json",package="pathlab")
    source<-if(nzchar(sourcefile))jsonlite::fromJSON(sourcefile,simplifyVector=FALSE) else NULL
    logistic<-identical(settings$outcome_family,"logistic")
    residual_correlations<-if(logistic)list(status="not_applicable",reason="Pearson OLS residual correlations are not defined for this mixed linear/logistic equation set; no logistic residual definition is substituted.") else cor(do.call(cbind,lapply(equations,function(e)e$fit$residuals)))
    list(schema_version=1,engine="pathlab-r/0.4.6",association_scale=if(logistic)"log_odds_statistical_path_product" else "outcome_units",graph=spec,requested_settings=settings,sample=sample[c("source_n","n","included_rows","exclusions","probes","extrapolation")],coding=sample$coding_rows,equations=equations,
      estimands=boot$rows,bootstrap=boot$metadata,bootstrap_distribution=boot$distribution,accepted_draws=boot$accepted_draws,
      slopes=diagnostics$slopes,johnson_neyman=diagnostics$johnson_neyman,interaction_tests=pl_interaction_tests(equations,robust),residual_correlations=residual_correlations,
      provenance=list(R=R.version.string,module="0.4.6",source=source,robust=robust,alpha=alpha,missing="One fixed complete-case sample across all nodes; no transformations",unit=if(logistic)"Log-odds statistical path products; not natural causal, probability, marginal total effects or odds ratios of indirect effects. Logistic noncollapsibility prevents interpretation as a reduced total coefficient." else "Statistical outcome-unit path associations; independent observations assumed"))
}
