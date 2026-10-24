#' Convert native controls to the canonical graph without changing selected paths
#' @export
native_spec <- function(options) {
    mode<-options$modelMode
    pl_require(is.null(mode) || (is.character(mode) && length(mode)==1 && !is.na(mode) && mode %in% c("legacy","general")),"INVALID_OPTIONS","Choose the two-mediator controls or the general graph editor")
    spec<-if(identical(mode,"general"))pl_native_general_spec(options) else pl_native_legacy_spec(options)
    coding<-pl_native_categorical_rows(options,spec)
    for(i in seq_along(spec$nodes))if(spec$nodes[[i]]$id %in% names(coding))spec$nodes[[i]]$measurement_scale<-"nominal"
    spec
}

pl_native_categorical_rows <- function(options,spec) {
    rows<-options$categoricalCoding
    if(is.null(rows))return(list())
    pl_require(is.list(rows) && length(rows)<=24,"INVALID_OPTIONS","Supply at most 24 categorical coding rows")
    slots<-c(X="x",W="w",Z="z",V="v",stats::setNames(paste0("cov",1:20),paste0("C",1:20)))
    ids<-pl_ids(spec$nodes);out<-list()
    for(row in rows) {
        pl_require(pl_keys(row,c("role","reference")) && is.character(row$role) && length(row$role)==1 && row$role %in% names(slots) && is.character(row$reference) && length(row$reference)==1 && !is.na(row$reference) && nzchar(row$reference),"INVALID_OPTIONS","Every categorical coding row needs an assigned role and an exact reference level")
        id<-unname(slots[[row$role]])
        pl_require(id %in% ids,"INCOMPLETE_MODEL",paste("Categorical role",row$role,"has no assigned column"))
        pl_require(!(id %in% names(out)),"INVALID_OPTIONS","Remove duplicate categorical coding rows")
        out[[id]]<-row$reference
    }
    out
}

# Category levels come from the delivered column's native level order. A
# numerical column becomes categorical only through an explicit coding row.
pl_native_levels <- function(value) {
    if(is.factor(value))return(levels(value))
    if(is.character(value))return(sort(unique(value[!is.na(value)]),method="radix"))
    pl_require(is.numeric(value),"NUMERIC_TYPE_MISMATCH","Categorical columns must contain numerical codes or text labels")
    sort(unique(value[!is.na(value)]))
}

pl_native_reference <- function(text,levels) {
    reference<-if(is.character(levels))text else suppressWarnings(as.numeric(text))
    pl_require(length(reference)==1 && !is.na(reference) && reference %in% levels && (is.character(reference)||is.finite(reference)),"INVALID_OPTIONS","The reference must exactly match one of the column's declared category levels")
    reference
}

pl_native_prepare_data <- function(data,spec,options) {
    family<-if(is.null(options$outcomeFamily))"linear" else options$outcomeFamily
    binary<-NULL
    for(i in seq_along(spec$nodes)) {
        node<-spec$nodes[[i]];value<-data[[node$column]]
        if(is.factor(value)||is.character(value))spec$nodes[[i]]$measurement_scale<-"nominal"
        if(node$role=="mediator")pl_require(is.numeric(value)&&!is.factor(value),"UNSUPPORTED_FEATURE","Mediators must be prepared continuous numerical scores; categorical mediators are not supported")
        if(node$role=="dependent" && family=="logistic") {
            levels<-pl_native_levels(value)
            pl_require(length(levels)==2,"INVALID_OPTIONS","A binary logistic outcome needs exactly two observed levels")
            event_text<-options$logisticEvent
            if(is.null(event_text))event_text<-""
            pl_require(is.character(event_text)&&length(event_text)==1&&!is.na(event_text),"INVALID_OPTIONS","Select the logistic event level")
            event<-if(!nzchar(event_text)) {
                pl_require(setequal(as.character(levels),c("0","1")),"INVALID_OPTIONS","Enter the exact event level for this binary outcome; blank is only defined for levels 0 and 1")
                if(is.character(levels))"1" else 1
            } else pl_native_reference(event_text,levels)
            observed<-if(is.factor(value))as.character(value) else value
            data[[node$column]]<-ifelse(is.na(observed),NA_real_,as.numeric(observed==event))
            binary<-list(node_id=node$id,column=node$column,levels=levels,event=event,non_event=levels[levels!=event],mapping="Declared event = 1; other level = 0; missing stays missing")
        }
    }
    list(data=data,spec=spec,binary=binary)
}

pl_native_legacy_spec <- function(options) {
    node_ids<-c("x","m1","m2","y","w")
    assigned<-function(value) is.character(value) && length(value)==1 && !is.na(value) && nzchar(value)
    for(id in node_ids) {
        value<-options[[id]]
        pl_require(is.null(value) || (is.character(value) && length(value)<=1 && !anyNA(value)),"INCOMPLETE_MODEL",paste("Choose one column for",toupper(id),"or leave an optional role empty"))
    }
    pl_require(all(vapply(c("x","y"),function(id)assigned(options[[id]]),TRUE)),"INCOMPLETE_MODEL","Select X and Y")
    node_ids<-node_ids[vapply(node_ids,function(id)assigned(options[[id]]),TRUE)]
    roles<-c(x="independent",m1="mediator",m2="mediator",y="dependent",w="moderator")
    nodes<-lapply(node_ids,function(i)list(id=i,column=options[[i]],role=unname(roles[i]),measurement_scale="interval"))
    cov_options<-c(m1="covM1",m2="covM2",y="covY")
    covariates<-lapply(cov_options,function(name) {
        value<-options[[name]]
        if(is.null(value))return(character())
        pl_require(is.character(value) && !anyNA(value) && all(nzchar(value)) && !anyDuplicated(value),"INVALID_OPTIONS",paste("Covariates in",name,"must be distinct column names"))
        value
    })
    for(target in names(covariates)) if(length(covariates[[target]])) pl_require(target %in% node_ids,"INCOMPLETE_MODEL",paste("Covariates assigned to",toupper(target),"require that mediator. Assign its column or explicitly clear that covariate list."))
    cov_columns<-sort(unique(unlist(covariates,use.names=FALSE)),method="radix")
    pl_require(length(cov_columns)<=20,"RESOURCE_LIMIT","At most twenty distinct continuous covariates are supported")
    pl_require(!any(cov_columns %in% vapply(nodes,`[[`,"","column")),"INVALID_MODEL","A covariate column cannot also be X, Y, a mediator or W")
    cov_ids<-stats::setNames(paste0("cov",seq_along(cov_columns)),cov_columns)
    for(column in cov_columns) nodes[[length(nodes)+1L]]<-list(id=unname(cov_ids[[column]]),column=column,role="covariate",measurement_scale="interval")
    pairs<-list(c("x","m1"),c("x","m2"),c("m1","m2"),c("x","y"),c("m1","y"),c("m2","y"),c("w","m1"),c("w","m2"),c("w","y"))
    edges<-list(); mods<-list()
    selected<-function(name) {
        value<-options[[name]]
        pl_require(is.null(value) || (is.logical(value) && length(value)==1 && !is.na(value)),"INVALID_OPTIONS",paste("Arrow or moderation control",name,"must be true or false"))
        isTRUE(value)
    }
    for(p in pairs) if(selected(paste("p",p[1],p[2],sep="_"))) {
        missing<-setdiff(p,node_ids)
        pl_require(!length(missing),"INCOMPLETE_MODEL",paste("Selected arrow",paste(toupper(p),collapse=" -> "),"requires",paste(toupper(missing),collapse=", "),". Assign its column or explicitly deselect this arrow."))
        edges[[length(edges)+1L]]<-list(id=paste("p",p[1],p[2],sep="_"),source=p[1],target=p[2])
    }
    for(target in names(covariates)) for(column in sort(covariates[[target]],method="radix")) {
        id<-unname(cov_ids[[column]])
        edges[[length(edges)+1L]]<-list(id=paste("p",id,target,sep="_"),source=id,target=target)
    }
    for(p in pairs[1:6]) if(selected(paste("mod",p[1],p[2],sep="_"))) {
        required<-c(p,"w"); missing<-setdiff(required,node_ids)
        pl_require(!length(missing),"INCOMPLETE_MODEL",paste("Selected moderation of",paste(toupper(p),collapse=" -> "),"requires",paste(toupper(missing),collapse=", "),". Assign its column or explicitly deselect this moderation."))
        pl_require(paste("p",p[1],p[2],sep="_") %in% pl_ids(edges),"INCOMPLETE_MODEL",paste("Selected moderation requires its explicit arrow",paste(toupper(p),collapse=" -> ")))
        pl_require(paste("p_w",p[2],sep="_") %in% pl_ids(edges),"INCOMPLETE_MODEL",paste("Selected moderation requires the explicit main-effect arrow W ->",toupper(p[2])))
        mods[[length(mods)+1L]]<-list(id=paste("mod",p[1],p[2],sep="_"),path_id=paste("p",p[1],p[2],sep="_"),moderator_ids=list("w"))
    }
    list(schema_version=1,nodes=nodes,edges=edges,moderations=mods)
}

# Ordered variable lists define stable role slots; arrows are never inferred.
pl_native_general_spec <- function(options) {
    one<-function(name) {
        value<-options[[name]]
        pl_require(is.character(value) && length(value)==1 && !is.na(value) && nzchar(value),"INCOMPLETE_MODEL",paste("Select a continuous column for",toupper(name)))
        value
    }
    many<-function(name,limit,label) {
        value<-options[[name]]
        if(is.null(value))return(character())
        pl_require(is.character(value) && !anyNA(value) && all(nzchar(value)) && !anyDuplicated(value),"INVALID_OPTIONS",paste(label,"must be distinct column names"))
        pl_require(length(value)<=limit,"RESOURCE_LIMIT",paste("At most",limit,label,"are supported"))
        value
    }
    columns<-c(x=one("x"),y=one("y"))
    mediators<-many("generalMediators",10,"mediators")
    moderators<-many("generalModerators",3,"moderators")
    covariates<-many("generalCovariates",20,"covariates")
    append_roles<-function(ids,values) if(length(values))stats::setNames(values,ids) else character()
    columns<-c(columns,append_roles(paste0("m",seq_along(mediators)),mediators),append_roles(c("w","z","v")[seq_along(moderators)],moderators),append_roles(paste0("cov",seq_along(covariates)),covariates))
    pl_require(!anyDuplicated(unname(columns)),"INVALID_MODEL","Each column can occupy only one role. Clear duplicate X, Y, mediator, moderator or covariate assignments.")
    ids<-names(columns)
    roles<-c(x="independent",y="dependent",append_roles(paste0("m",seq_along(mediators)),rep("mediator",length(mediators))),append_roles(c("w","z","v")[seq_along(moderators)],rep("moderator",length(moderators))),append_roles(paste0("cov",seq_along(covariates)),rep("covariate",length(covariates))))
    nodes<-lapply(ids,function(id)list(id=id,column=unname(columns[[id]]),role=unname(roles[[id]]),measurement_scale="interval"))
    slots<-c(X="x",Y="y",stats::setNames(paste0("m",1:10),paste0("M",1:10)),W="w",Z="z",V="v",stats::setNames(paste0("cov",1:20),paste0("C",1:20)))
    rows<-function(name,keys) {
        value<-options[[name]]
        if(is.null(value))return(list())
        pl_require(is.list(value) && length(value)<=150,"RESOURCE_LIMIT",paste(name,"must be a list with at most 150 rows"))
        # Native ComboBoxes display their first choice for an untouched cell,
        # but the Array/Group wire value retains NULL. Resolve only those named
        # NULL cells to the choice the researcher actually sees. Missing keys,
        # empty strings and invalid values must still fail validation.
        defaults<-switch(name,generalPaths=list(source="X",target="M1"),generalModerations=list(source="X",target="M1",mod1="W",mod2="none"))
        value<-lapply(seq_along(value),function(i) {
            row<-value[[i]]
            pl_require(pl_keys(row,keys),"INVALID_OPTIONS",paste("Complete every cell in",name,"row",i))
            for(key in keys)if(is.null(row[[key]]))row[key]<-defaults[key]
            pl_require(all(vapply(row,function(v)is.character(v) && length(v)==1 && !is.na(v) && nzchar(v),TRUE)),"INVALID_OPTIONS",paste("Complete every cell in",name,"row",i))
            row
        })
        value
    }
    role_id<-function(slot) {
        pl_require(slot %in% names(slots),"INVALID_OPTIONS",paste("Unknown role slot",slot))
        id<-unname(slots[[slot]])
        pl_require(id %in% ids,"INCOMPLETE_MODEL",paste("Selected role",slot,"has no assigned column. Assign that slot or explicitly remove its arrow/interaction row."))
        id
    }
    edges<-lapply(rows("generalPaths",c("source","target")),function(row) {
        source<-role_id(row$source); target<-role_id(row$target)
        pl_require(source!=target && roles[[source]]!="dependent" && roles[[target]] %in% c("mediator","dependent"),"INVALID_MODEL","Arrows must run from X, a mediator, moderator or covariate into a different mediator or Y")
        list(id=paste("p",source,target,sep="_"),source=source,target=target)
    })
    pl_require(length(edges)>0,"INCOMPLETE_MODEL","Add each intended arrow in the explicit path table. No arrows are added automatically.")
    pl_require(!anyDuplicated(pl_ids(edges)),"INVALID_MODEL","Remove duplicate arrow rows")
    mods<-lapply(rows("generalModerations",c("source","target","mod1","mod2")),function(row) {
        source<-role_id(row$source); target<-role_id(row$target)
        mm<-c(row$mod1,if(row$mod2!="none")row$mod2)
        pl_require(all(mm %in% c("W","Z","V")) && !anyDuplicated(mm),"INVALID_MODEL","Choose one or two distinct moderators in each interaction row")
        mm<-vapply(mm,role_id,""); mm<-c("w","z","v")[c("w","z","v") %in% mm]
        pl_require(!(source %in% mm),"INVALID_MODEL","A path source cannot moderate itself")
        path<-paste("p",source,target,sep="_")
        pl_require(path %in% pl_ids(edges),"INCOMPLETE_MODEL",paste("Interaction requires its explicit arrow",row$source,"->",row$target))
        for(mod in mm)pl_require(paste("p",mod,target,sep="_") %in% pl_ids(edges),"INCOMPLETE_MODEL",paste("Interaction requires the explicit main-effect arrow",toupper(mod),"->",row$target))
        list(id=paste(c("mod",source,target,mm),collapse="_"),path_id=path,moderator_ids=as.list(mm))
    })
    list(schema_version=1,nodes=nodes,edges=edges,moderations=mods)
}

native_settings <- function(options,spec,data=NULL) {
    family<-if(is.null(options$outcomeFamily))"linear" else options$outcomeFamily
    pl_require(is.character(family)&&length(family)==1&&family %in% c("linear","logistic"),"INVALID_OPTIONS","Choose a continuous linear or binary logistic outcome")
    settings<-list(schema_version=3,outcome_family=family,categorical=list(),probes=list(),automatic_probes=list(probabilities=c(.16,.5,.84),quantile_method="linear",sample="fixed_complete_case_sample",resolve_before_bootstrap=TRUE),bootstrap_interval_quantile_method="linear")
    coding<-pl_native_categorical_rows(options,spec)
    by<-setNames(spec$nodes,pl_ids(spec$nodes))
    for(id in names(coding)) {
        pl_require(is.data.frame(data),"INVALID_DATA","The selected categorical columns are needed to resolve their level order")
        levels<-pl_native_levels(data[[by[[id]]$column]])
        pl_require(length(levels)>=2 && length(levels)<=10,"INVALID_OPTIONS","Categorical variables require two to ten declared levels")
        settings$categorical[[length(settings$categorical)+1L]]<-list(node_id=id,levels=levels,reference=pl_native_reference(coding[[id]],levels))
    }
    if(is.data.frame(data))for(node in spec$nodes)if(node$role %in% c("independent","moderator","covariate") && (is.factor(data[[node$column]])||is.character(data[[node$column]])))pl_require(node$id %in% names(coding),"INVALID_OPTIONS",paste("Add an explicit categorical coding row and reference level for",node$column,"; category codes are never treated as continuous scores automatically"))
    controls<-if(identical(options$modelMode,"general"))c(w="generalProbesW",z="generalProbesZ",v="generalProbesV") else c(w="probes")
    moderators<-pl_ids(Filter(function(n)n$role=="moderator",spec$nodes))
    for(id in names(controls)) {
        value<-options[[controls[[id]]]]
        if(is.null(value))next
        pl_require(is.character(value) && length(value)==1 && !is.na(value),"INVALID_OPTIONS","Probe values must be comma-separated numbers or declared category labels")
        value<-trimws(value)
        if(!nzchar(value))next
        pl_require(id %in% moderators,"INVALID_OPTIONS",paste("Custom probes require an assigned moderator",toupper(id)))
        declaration<-Filter(function(row)row$node_id==id,settings$categorical)
        if(!length(declaration))pl_require(!grepl("(^,|,$|,[[:space:]]*,)",value),"INVALID_OPTIONS","Supply one to five distinct comma-separated numeric probes without empty entries")
        vals<-if(length(declaration)) {
            parsed<-tryCatch(utils::read.csv(text=value,header=FALSE,colClasses="character",check.names=FALSE,strip.white=TRUE,na.strings=NULL,comment.char=""),error=function(e)pl_error("INVALID_OPTIONS","Category probes must be comma-separated labels; quote labels containing commas"))
            pl_require(nrow(parsed)==1,"INVALID_OPTIONS","Supply category probes on one line")
            levels<-declaration[[1]]$levels;v<-unname(unlist(parsed[1,,drop=FALSE]))
            if(!is.character(levels))v<-suppressWarnings(as.numeric(v))
            pl_require(all(v %in% levels),"INVALID_OPTIONS","Every categorical probe must be one of the declared levels")
            v
        } else suppressWarnings(as.numeric(trimws(strsplit(value,",",fixed=TRUE)[[1]])))
        pl_require(length(vals)%in%1:5 && !anyNA(vals) && (is.character(vals)||all(is.finite(vals))) && !anyDuplicated(vals),"INVALID_OPTIONS","Supply one to five distinct valid probe values")
        settings$probes[[length(settings$probes)+1L]]<-list(node_id=id,values=vals)
    }
    settings
}

pl_public_result <- function(result) {
    result$bootstrap_distribution<-NULL
    result$equations<-lapply(result$equations,function(e) {
        e$design<-NULL; e$y<-NULL; e$fit$residuals<-NULL; e$fit$fitted<-NULL
        e$fit$leverage<-NULL; e$fit$linear_predictors<-NULL;e$fit$weights<-NULL;e
    })
    result
}

