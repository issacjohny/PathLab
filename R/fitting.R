pl_fit <- function(X,y,robust=FALSE,alpha=.05,coefficients_only=FALSE,family="linear") {
    n<-nrow(X); p<-ncol(X)
    pl_require(n>p+2,"SINGULAR_DESIGN","Insufficient residual degrees of freedom")
    pl_require(family %in% c("linear","logistic"),"INVALID_OPTIONS","Unknown equation family")
    if(family=="logistic") return(pl_fit_logistic(X,y,robust,alpha,coefficients_only))
    fit<-stats::lm.fit(X,y,tol=1e-10)
    pl_require(fit$rank==p && all(is.finite(fit$coefficients)),"SINGULAR_DESIGN")
    beta<-fit$coefficients
    if(coefficients_only) return(beta)
    Q<-qr.Q(fit$qr); leverage<-rowSums(Q^2); residual<-fit$residuals
    # QR inverse in original column order, including pivoting.
    inv<-chol2inv(qr.R(fit$qr)); bread<-matrix(0,p,p); bread[fit$qr$pivot,fit$qr$pivot]<-inv
    pl_require(all(leverage<1-1e-10),"SINGULAR_DESIGN","At least one case has unit leverage; check singleton joint categorical cells or an overfitted equation")
    df<-n-p; sse<-sum(residual^2); sst<-sum((y-mean(y))^2)
    covariance<-if(robust) bread %*% crossprod(X,X*(residual/(1-leverage))^2) %*% bread else bread*sse/df
    se<-sqrt(pmax(0,diag(covariance))); z<-beta/se
    critical<-if(robust) qnorm(1-alpha/2) else qt(1-alpha/2,df)
    prob<-2*if(robust) pnorm(-abs(z)) else pt(-abs(z),df)
    slopebeta<-beta[-1]; slopecov<-covariance[-1,-1,drop=FALSE]
    wald<-as.numeric(crossprod(slopebeta,solve(slopecov,slopebeta)))
    # statsmodels OLS uses a residual-df Wald F for the overall HC3 model test,
    # while its individual HC3 coefficients retain normal references.
    stat<-wald/(p-1)
    list(coefficients=beta,covariance=covariance,se=se,statistic=z,p=prob,lower=beta-critical*se,upper=beta+critical*se,
         reference=if(robust)"normal (HC3)" else "t",df_residual=df,residuals=residual,fitted=fit$fitted.values,
         leverage=leverage,r_squared=1-sse/sst,rmse=sqrt(sse/df),model_test=list(statistic=stat,df1=p-1,df2=df,p=pf(stat,p-1,df,lower.tail=FALSE),reference=if(robust)"HC3 Wald F" else "OLS F"))
}

# A bounded primal simplex checks the same separation cone as the Python
# engine: signed design margins >= 0 with a strictly positive mean margin.
# Splitting each free coefficient into two nonnegative variables bounded by
# one loses no possible direction (any separating direction can be rescaled).
# The initial all-zero coefficients are feasible, so no artificial phase is
# needed. Bland's rule handles the many zero-margin, degenerate constraints.
pl_logistic_separation <- function(signed,max_iterations=50000L,diagnostics=FALSE,max_work=2e8) {
    p<-ncol(signed); variables<-2L*p
    m<-nrow(signed)+variables;last<-variables+1L
    # Each pivot visits a dense tableau of this size. Bound that work as well
    # as the iteration count, including for a very tall, low-dimensional fit.
    # Exhaustion is a failed certificate, never evidence of overlap.
    pl_require(length(max_work)==1L && is.numeric(max_work) && is.finite(max_work) && max_work>=0 &&
        length(max_iterations)==1L && is.numeric(max_iterations) && is.finite(max_iterations) && max_iterations>=0 && max_iterations==floor(max_iterations),
        "INVALID_OPTIONS","Invalid separation-check work limit")
    work_per_iteration<-as.double(m+1L)*as.double(last)
    work_iterations<-floor(max_work/work_per_iteration)
    limit<-min(max_iterations,work_iterations)
    if(limit<1L)pl_error("NONCONVERGENCE",if(work_iterations<1L)"Separation linear program exceeded its tableau work budget" else "Separation linear program exceeded its iteration limit")
    constraints<-rbind(cbind(-signed,signed),diag(variables))
    rhs<-c(rep(0,nrow(signed)),rep(1,variables))
    objective<-c(colMeans(signed),-colMeans(signed))
    tab<-rbind(cbind(constraints,rhs),c(-objective,0))
    basic<-variables+seq_len(m); nonbasic<-seq_len(variables)
    tolerance<-1e-10
    for(iteration in seq_len(limit)) {
        entering<-which(tab[m+1L,seq_len(variables)] < -tolerance)
        if(!length(entering)) {
            solution<-numeric(variables)
            selected<-which(basic<=variables)
            solution[basic[selected]]<-tab[selected,last]
            # Re-solve the small active basis against the original constraints
            # to remove accumulated tableau roundoff before certification.
            slacks<-which(nonbasic>variables);binding<-nonbasic[slacks]-variables
            active_variables<-basic[selected]
            active<-constraints[binding,active_variables,drop=FALSE]
            if(length(active_variables)) {
                solution[active_variables]<-tryCatch(as.numeric(solve(active,rhs[binding])),
                    error=function(e)pl_error("NONCONVERGENCE","Separation active basis could not be refined"))
            }
            direction<-solution[seq_len(p)]-solution[p+seq_len(p)]
            margin<-as.numeric(signed%*%direction)
            margin_roundoff<-64*.Machine$double.eps*pmax(1,as.numeric(abs(signed)%*%abs(direction)))
            if(!all(is.finite(margin)) || any(margin< -margin_roundoff))
                pl_error("NONCONVERGENCE","Separation linear program did not produce a feasible certificate")
            average<-mean(margin)
            # Slack reduced costs supply a dual certificate. Basic slacks have
            # zero dual weight; a nonbasic slack's objective coefficient is its
            # constraint multiplier. This remains internal audit information.
            dual<-numeric(m)
            if(length(binding)) dual[binding]<-tryCatch(as.numeric(solve(t(active),objective[active_variables])),
                error=function(e)pl_error("NONCONVERGENCE","Separation dual basis could not be refined"))
            dual_slack<-as.numeric(crossprod(constraints,dual))-objective
            dual_roundoff<-64*.Machine$double.eps*pmax(1,as.numeric(crossprod(abs(constraints),abs(dual)))+abs(objective))
            bound_roundoff<-64*.Machine$double.eps*pmax(1,abs(solution))
            audit<-list(direction=direction,margins=margin,split_parameters=solution,
                objective=average,dual_multipliers=dual,dual_slack=dual_slack,
                dual_objective=sum(rhs*dual),iterations=iteration,iteration_limit=limit,
                tableau_cells=work_per_iteration,work_upper_bound=iteration*work_per_iteration,work_budget=max_work,
                primal_feasible=all(margin>= -margin_roundoff) && all(solution>= -bound_roundoff) && all(solution<=1+bound_roundoff),
                dual_feasible=all(dual>= -64*.Machine$double.eps*pmax(1,abs(dual))) && all(dual_slack>= -dual_roundoff))
            gap_roundoff<-64*.Machine$double.eps*max(1,sum(abs(rhs*dual)),abs(average))
            if(!audit$primal_feasible || !audit$dual_feasible || abs(audit$dual_objective-average)>gap_roundoff)
                pl_error("NONCONVERGENCE","Separation linear program failed its primal/dual certificate check")
            if(average>64*.Machine$double.eps*max(1,mean(abs(margin)))) {
                margin<-margin/average
                if(min(margin)>= -1e-7 && mean(margin)>=1-1e-7) return(if(diagnostics)c(list(separated=TRUE),audit) else TRUE)
                pl_error("NONCONVERGENCE","Separation certificate was numerically unstable")
            }
            if(average>0)
                pl_error("NONCONVERGENCE","Separation direction has a positive margin below certifiable numerical precision")
            return(if(diagnostics)c(list(separated=FALSE),audit) else FALSE)
        }
        column<-entering[which.min(nonbasic[entering])]
        leaving<-which(tab[seq_len(m),column]>tolerance)
        if(!length(leaving)) pl_error("NONCONVERGENCE","Bounded separation linear program became unbounded")
        ratio<-tab[leaving,last]/tab[leaving,column]
        minimum<-min(ratio)
        tied<-leaving[abs(ratio-minimum)<=tolerance]
        row<-tied[which.min(basic[tied])]
        pivot<-tab[row,column]
        other_rows<-setdiff(seq_len(m+1L),row); other_columns<-setdiff(seq_len(last),column)
        tab[other_rows,other_columns]<-tab[other_rows,other_columns,drop=FALSE]-outer(tab[other_rows,column],tab[row,other_columns])/pivot
        tab[row,other_columns]<-tab[row,other_columns]/pivot
        tab[other_rows,column]<- -tab[other_rows,column]/pivot
        tab[row,column]<-1/pivot
        old<-basic[row];basic[row]<-nonbasic[column];nonbasic[column]<-old
        if(any(!is.finite(tab))) pl_error("NONCONVERGENCE","Separation linear program overflowed")
    }
    pl_error("NONCONVERGENCE",if(work_iterations<max_iterations)"Separation linear program exceeded its tableau work budget" else "Separation linear program exceeded its iteration limit")
}

pl_fit_logistic <- function(X,y,robust,alpha,coefficients_only) {
    n<-nrow(X); p<-ncol(X)
    rank_full<-function(matrix) {
        singular<-svd(matrix,nu=0,nv=0)$d
        length(singular)==ncol(matrix) && all(is.finite(singular)) && min(singular)>max(dim(matrix))*.Machine$double.eps*max(singular)
    }
    pl_require(all(is.finite(X)) && rank_full(X),"SINGULAR_DESIGN")
    pl_require(is.numeric(y) && length(y)==n && all(is.finite(y)) && setequal(unique(y),c(0,1)),"INVALID_OPTIONS","A logistic outcome must contain both numerical values 0 and 1")
    scales<-apply(abs(X),2,max); Z<-sweep(X,2,scales,"/")
    if(!coefficients_only && pl_logistic_separation(Z*(2*y-1)))
        pl_error("SEPARATION","The logistic equation has complete or quasi-complete separation")
    parameters<-numeric(p); converged<-FALSE
    for(iteration in seq_len(100L)) {
        probability<-stats::plogis(as.numeric(Z%*%parameters))
        weight<-probability*(1-probability)
        information<-crossprod(Z,Z*weight)
        pl_require(all(is.finite(information)) && rank_full(information),"SINGULAR_DESIGN","The logistic weighted information is singular")
        step<-tryCatch(as.numeric(solve(information,crossprod(Z,y-probability))),error=function(e)pl_error("SINGULAR_DESIGN","The logistic Newton system is singular"))
        parameters<-parameters+step
        pl_require(all(is.finite(parameters)),"NUMERICAL_OVERFLOW","Logistic coefficients overflowed")
        if(all(abs(stats::plogis(as.numeric(Z%*%parameters))-y)<=1e-8))
            pl_error("SEPARATION","Fitted logistic probabilities perfectly predict the outcome")
        if(max(abs(step))<=1e-8) { converged<-TRUE;break }
    }
    pl_require(converged,"NONCONVERGENCE","The logistic fit did not converge within 100 Newton iterations")
    beta<-parameters/scales; names(beta)<-colnames(X)
    pl_require(all(is.finite(beta)),"NUMERICAL_OVERFLOW")
    eta<-as.numeric(Z%*%parameters);probability<-stats::plogis(eta)
    weight<-probability*(1-probability);information<-crossprod(Z,Z*weight)
    pl_require(all(is.finite(information)) && rank_full(information),"SINGULAR_DESIGN","The logistic weighted information is singular")
    if(coefficients_only) return(beta)
    bread<-solve(information)
    leverage<-weight*rowSums((Z%*%bread)*Z)
    pl_require(all(is.finite(leverage)) && all(leverage<1-1e-10),"SINGULAR_DESIGN")
    residual<-y-probability
    covariance<-if(robust)bread%*%crossprod(Z,Z*(residual/(1-leverage))^2)%*%bread else bread
    covariance<-covariance/outer(scales,scales)
    dimnames(covariance)<-list(colnames(X),colnames(X))
    se<-sqrt(pmax(0,diag(covariance))); statistic<-beta/se;critical<-stats::qnorm(1-alpha/2)
    # Stable Bernoulli log likelihood in the original model, without clipping.
    softplus<-function(value)pmax(value,0)+log1p(exp(-abs(value)))
    likelihood<-sum(y*eta-softplus(eta)); proportion<-mean(y)
    null_likelihood<-sum(y*log(proportion)+(1-y)*log1p(-proportion))
    lr<-2*(likelihood-null_likelihood)
    list(coefficients=beta,covariance=covariance,se=se,statistic=statistic,p=2*stats::pnorm(-abs(statistic)),
         lower=beta-critical*se,upper=beta+critical*se,reference="normal z",family="logistic",df_model=p-1L,df_residual=n-p,
         residuals=residual,fitted=probability,linear_predictors=eta,weights=weight,leverage=leverage,
         r_squared=NULL,rmse=NULL,log_likelihood=likelihood,null_log_likelihood=null_likelihood,pseudo_R2=1-likelihood/null_likelihood,
         AIC=-2*likelihood+2*p,BIC=-2*likelihood+log(n)*p,
         model_test=list(statistic=lr,df1=p-1L,df2=NULL,p=stats::pchisq(lr,p-1L,lower.tail=FALSE),reference="likelihood-ratio chi-square (model-based)"),
         convergence=list(converged=TRUE,iterations=iteration,max_iterations=100L,parameter_tolerance=1e-8,method="Newton on column-scaled uncentered design"),
         separation_check=if(coefficients_only)"Bootstrap fits check convergence, finite parameters and weighted information rank" else "Original sample checked for complete/quasi-complete separation using unit-scaled signed-margin linear-program feasibility; finite coefficients, convergence and full-rank weighted information are also required")
}

#' Fit the compiled observed equations
#' @export
fit_equations <- function(equations,robust=TRUE,alpha=.05) lapply(equations,function(e) c(e,list(fit=pl_fit(e$design,e$y,robust,alpha,family=if(is.null(e$family))"linear" else e$family))))

pl_slope_vector <- function(edge,equation,condition,derivative=character(),focal_factor=NULL) {
    if(is.null(focal_factor)) focal_factor<-if(length(equation$basis))equation$basis[[edge$source]][1] else edge$source
    at<-function(factor) {
        description<-equation$basis_description[[factor]]
        node<-if(length(description))description$node_id else factor
        if(!is.null(description$level)) {
            value<-condition[[node]]
            if(is.null(value))value<-equation$coding[[node]]$reference
            return(as.numeric(value==description$level))
        }
        value<-condition[[node]]
        if(is.null(value))value<-equation$means[[node]]
        if(is.null(value))return(0)
        as.numeric(value)
    }
    vapply(equation$terms,function(f) {
        if(!(focal_factor %in% f)) return(0)
        other<-setdiff(f,focal_factor)
        if(anyDuplicated(derivative) || !all(derivative %in% other)) return(0)
        remaining<-setdiff(other,derivative)
        prod(vapply(remaining,at,0.0))
    },0.0)
}

pl_slopes <- function(model,sample,equations,alpha) {
    rows<-list(); jn<-list()
    means<-lapply(sample$values[setdiff(names(model$by),names(model$coding))],mean)
    references<-lapply(model$coding,`[[`,"reference")
    profile<-function(condition) { out<-c(means,references);out[names(condition)]<-condition;out }
    distinct<-function(conditions,omit=character()) {
        conditions<-lapply(conditions,function(c)c[setdiff(names(c),omit)])
        key<-function(c)jsonlite::toJSON(c[order(names(c))],auto_unbox=TRUE,digits=NA)
        conditions[!duplicated(vapply(conditions,key,""))]
    }
    conditioning_note<-"Other numerical equation predictors are held at complete-case means; other categorical predictors at their declared reference. All equation terms involving the focal source are included."
    for(e in model$spec$edges) {
        eq<-equations[[e$target]]
        mm<-unique(unlist(lapply(Filter(function(m)m$path_id==e$id,model$spec$moderations),`[[`,"moderator_ids")))
        if(!length(mm)) next
        predictors<-unique(vapply(unique(unlist(eq$terms)),function(f)if(length(eq$basis_description))eq$basis_description[[f]]$node_id else f,""))
        held<-function(condition,varying=character())means[setdiff(predictors,c(e$source,names(condition),varying))]
        held_reference<-function(condition,varying=character())references[intersect(names(references),setdiff(predictors,c(e$source,names(condition),varying)))]
        conditions<-distinct(sample$conditions,e$source)
        factors<-if(length(sample$basis))sample$basis[[e$source]] else e$source
        for(factor in factors) {
        focal<-if(!is.null(sample$basis_description[[factor]]$level))sample$basis_description[[factor]] else NULL
        for(c in conditions) {
            v<-pl_slope_vector(e,eq,profile(c),focal_factor=factor); f<-eq$fit; b<-sum(v*f$coefficients); se<-sqrt(as.numeric(t(v)%*%f$covariance%*%v))
            critical<-if(f$reference=="t")qt(1-alpha/2,f$df_residual) else qnorm(1-alpha/2)
            rows[[length(rows)+1L]]<-list(path_id=e$id,source_id=e$source,target_id=e$target,condition=c,held_at_means=held(c),held_at_references=held_reference(c),focal_contrast=focal,scale=if(identical(eq$family,"logistic"))"log_odds" else "outcome_units",conditioning_note=conditioning_note,estimate=b,se=se,lower=b-critical*se,upper=b+critical*se,
              p=2*if(f$reference=="t")pt(-abs(b/se),f$df_residual) else pnorm(-abs(b/se)),reference=f$reference)
        }
        for(i in mm)for(other in distinct(conditions,i)) {
            if(i %in% names(model$coding)) {
                jn[[length(jn)+1L]]<-list(path_id=e$id,moderator_id=i,other_conditions=other,focal_contrast=focal,status="not_applicable",roots=numeric(),boundaries=numeric(),regions=list(),observed_range=NULL,reason="Categorical moderators use level comparisons, not numerical Johnson-Neyman regions.",conditioning_note=conditioning_note)
                next
            }
            zero<-profile(other);zero[[i]]<-0;one<-zero;one[[i]]<-1
            a<-pl_slope_vector(e,eq,zero,focal_factor=factor); b<-pl_slope_vector(e,eq,one,focal_factor=factor)-a; f<-eq$fit
            crit<-if(f$reference=="t")qt(1-alpha/2,f$df_residual) else qnorm(1-alpha/2)
            A<-sum(a*f$coefficients); B<-sum(b*f$coefficients); C<-f$covariance
            polynomial<-c(A^2-crit^2*as.numeric(t(a)%*%C%*%a),2*A*B-2*crit^2*as.numeric(t(a)%*%C%*%b),B^2-crit^2*as.numeric(t(b)%*%C%*%b))
            tolerance<-.Machine$double.eps*max(abs(polynomial))*32
            stable<-ifelse(abs(polynomial)<=tolerance,0,polynomial)
            degree<-if(any(stable!=0))max(which(stable!=0))-1L else 0L
            roots<-if(degree>0)polyroot(stable[seq_len(degree+1L)]) else complex()
            roots<-sort(unique(Re(roots[is.finite(roots) & abs(Im(roots))<1e-8])))
            observed<-range(sample$values[[i]]);boundaries<-roots[roots>observed[1] & roots<observed[2]]
            cuts<-c(observed[1],boundaries,observed[2])
            regions<-lapply(seq_len(length(cuts)-1L),function(k) {
                midpoint<-(cuts[k]+cuts[k+1L])/2
                list(lower=cuts[k],upper=cuts[k+1L],significant=sum(polynomial*midpoint^(0:2))>tolerance)
            })
            jn[[length(jn)+1L]]<-list(path_id=e$id,moderator_id=i,other_conditions=other,held_at_means=held(other,i),held_at_references=held_reference(other,i),focal_contrast=focal,status="available",roots=roots,boundaries=boundaries,regions=regions,observed_range=observed,
              polynomial=polynomial,conditioning_note=conditioning_note,definition="Pointwise analytic simple-slope region conditional on the stated other values: constant + linear*w + quadratic*w^2 > 0; not an indirect-effect region")
        }
        }
    }
    list(slopes=rows,johnson_neyman=jn)
}

pl_interaction_tests <- function(equations,robust) {
    out<-list()
    for(e in equations) {
        ii<-which(lengths(e$terms)>1)
        if(!length(ii))next
        f<-e$fit; b<-f$coefficients[ii]; V<-f$covariance[ii,ii,drop=FALSE]
        test<-tryCatch(as.numeric(crossprod(b,solve(V,b))),error=function(e)NA_real_)
        logistic<-identical(e$family,"logistic");classical_linear<-!robust && !logistic
        delta<-if(logistic)NULL else {
            restricted<-stats::lm.fit(e$design[,-ii,drop=FALSE],e$y)
            sum(restricted$residuals^2-f$residuals^2)/sum((e$y-mean(e$y))^2)
        }
        stat<-if(classical_linear)test/length(ii) else test
        out[[length(out)+1L]]<-list(target_id=e$target_id,statistic=stat,df_num=length(ii),df_den=if(classical_linear)f$df_residual else NULL,
          p=if(classical_linear)pf(stat,length(ii),f$df_residual,lower.tail=FALSE) else pchisq(stat,length(ii),lower.tail=FALSE),
          reference=if(robust)"HC3 Wald chi-square" else if(logistic)"model-based Wald chi-square" else "OLS partial F",deltaR2=delta,
          status=if(is.finite(stat))"computed" else "unavailable",reason=if(is.finite(stat))NULL else "Interaction-block covariance is singular")
    }
    out
}
