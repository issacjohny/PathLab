# A single native fit cache persists through jamovi's result state API.
# Only input fingerprints and fitted/public summaries are retained. No raw
# data values, design matrices, case residuals or bootstrap distributions.
pl_native_model_fields <- function() {
    c("modelMode","x","m1","m2","y","w","covM1","covM2","covY","probes","generalMediators","generalModerators","generalCovariates","generalPaths","generalModerations","generalProbesW","generalProbesZ","generalProbesV","categoricalCoding","outcomeFamily","logisticEvent",paste0("p_",c("x_m1","x_m2","m1_m2","x_y","m1_y","m2_y","w_m1","w_m2","w_y")),paste0("mod_",c("x_m1","x_m2","m1_m2","x_y","m1_y","m2_y")))
}
pl_native_presentation_fields <- function() {
    c("showDetails","showCovariance","showEffectPlot","showInteractionPlot","showJNPlot","plotEffectKind","plotEffectPage","plotSource","plotTarget","plotModerator","plotCondition","plotContrast","plotResponseScale","plotInteractionMode","plotInteractionPage")
}
pl_native_is_presentation <- function(fields) {
    # jamovi adds these six result options when users resize native images.
    # Match complete paths: an unknown statistical option must still invalidate.
    fields %in% pl_native_presentation_fields() |
        grepl("^results/(effectPlot|interactionPlot|jnPlot)/(widthScale|heightScale)$",fields) |
        grepl('^results/interactionPlots/"[1-9][0-9]*"/(widthScale|heightScale)$',fields)
}
pl_fit_cache_key <- function(data,controls,estimation) {
    sourcefile<-system.file("provenance","build-source.json",package="pathlab")
    source<-if(nzchar(sourcefile))jsonlite::fromJSON(sourcefile,simplifyVector=FALSE) else NULL
    # jamovi may reopen an unchanged Number option as integer (95L) rather
    # than double (95). Both specify the same confidence level. Normalize
    # only its storage type; retain the exact value and all other inputs.
    if(is.numeric(estimation$confidence))estimation$confidence<-as.double(estimation$confidence)
    # Include full delivered values, order, missingness and factor attributes,
    # not just N/means. This also guards against unannounced filter/data changes.
    digest::digest(list(schema=1L,engine="pathlab-r/0.4.6",R=R.version.string,
        platform=R.version$platform,source=source,data=data,controls=controls,
        estimation=estimation),algo="sha256",serializeVersion=2L)
}
pl_fit_cache_entry <- function(key,result) {
    result<-pl_public_result(result)
    result$plots<-NULL;result$plot_conditions<-NULL;result$fit_cache<-NULL
    payload<-digest::digest(result,algo="sha256",serializeVersion=2L)
    computed<-format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%OS6Z")
    list(schema=1L,key=key,result=result,payload_sha256=payload,
        fit_id=digest::digest(list(key,computed,Sys.getpid(),proc.time()),algo="sha256",serializeVersion=2L),
        computed_utc=computed,
        compressed_bytes=length(memCompress(serialize(result,NULL,version=2L),type="gzip")))
}
pl_fit_cache_read <- function(entry,key) {
    # Old, missing, incomplete or accidentally altered state is a cache miss.
    valid<-tryCatch(is.list(entry) && identical(entry$schema,1L) &&
        identical(entry$key,key) && is.list(entry$result) &&
        identical(entry$result$engine,"pathlab-r/0.4.6") &&
        is.character(entry$fit_id) && length(entry$fit_id)==1L &&
        identical(entry$payload_sha256,digest::digest(entry$result,algo="sha256",serializeVersion=2L)),
        error=function(e)FALSE)
    if(isTRUE(valid))entry else NULL
}

# jamovi 28 deliberately drops serialized R state on file import. Persist only
# plain data in hidden result text as well. This codec never evaluates text or
# unserializes R objects; exact numeric bytes and character encodings retain the
# payload hash, matrix attributes and integer/double distinctions.
pl_fit_cache_json <- function(entry) {
    tryCatch({
        pack<-function(x,depth=0L) {
            if(depth>64L)stop("cache depth")
            type<-typeof(x);attrs<-attributes(x)
            if(!type%in%c("NULL","list","double","integer","logical","character") ||
               any(!names(attrs)%in%c("names","dim","dimnames")))stop("cache type")
            value<-switch(type,
                "NULL"=NULL,
                "list"=unname(lapply(x,pack,depth=depth+1L)),
                "character"=unname(lapply(seq_along(x),function(i)if(is.na(x[i]))list(na=TRUE) else
                    list(bytes=jsonlite::base64_enc(charToRaw(x[i])),encoding=Encoding(x[i])))),
                jsonlite::base64_enc(writeBin(if(type=="double")as.double(x) else as.integer(x),raw(),
                    size=if(type=="double")8L else 4L,endian="little")))
            list(type=type,length=length(x),value=value,attributes=if(is.null(attrs))NULL else lapply(attrs,pack,depth=depth+1L))
        }
        text<-as.character(jsonlite::toJSON(list(format="pathlab-data-cache",schema=1L,payload=pack(entry)),
            auto_unbox=TRUE,null="null",digits=NA))
        if(nchar(text,type="bytes")>8*1024^2)"" else text
    },error=function(e)"")
}

pl_fit_cache_from_json <- function(text,key) {
    tryCatch({
        if(!is.character(text)||length(text)!=1L||is.na(text)||!nzchar(text)||nchar(text,type="bytes")>8*1024^2)return(NULL)
        wire<-jsonlite::fromJSON(text,simplifyVector=FALSE)
        if(!identical(wire$format,"pathlab-data-cache")||!identical(wire$schema,1L))return(NULL)
        nodes<-0L;values<-0
        unpack<-function(p,depth=0L) {
            nodes<<-nodes+1L
            if(depth>64L||nodes>100000L||!is.list(p)||!is.character(p$type)||length(p$type)!=1L||
               !p$type%in%c("NULL","list","double","integer","logical","character")||
               !is.numeric(p$length)||length(p$length)!=1L||!is.finite(p$length)||p$length<0||p$length!=floor(p$length))stop("cache shape")
            n<-p$length;values<<-values+n
            if(values>1000000)stop("cache budget")
            x<-switch(p$type,
                "NULL"={if(n!=0)stop("cache null");NULL},
                "list"={if(!is.list(p$value)||length(p$value)!=n)stop("cache list");lapply(p$value,unpack,depth=depth+1L)},
                "character"={
                    if(!is.list(p$value)||length(p$value)!=n)stop("cache strings")
                    vapply(p$value,function(v) {
                        if(identical(v$na,TRUE))return(NA_character_)
                        if(!is.character(v$bytes)||length(v$bytes)!=1L||!is.character(v$encoding)||length(v$encoding)!=1L||
                           !v$encoding%in%c("unknown","UTF-8","latin1","bytes"))stop("cache encoding")
                        s<-rawToChar(jsonlite::base64_dec(v$bytes));Encoding(s)<-v$encoding;s
                    },"")
                },{
                    if(!is.character(p$value)||length(p$value)!=1L)stop("cache numbers")
                    bytes<-jsonlite::base64_dec(p$value);size<-if(p$type=="double")8L else 4L
                    if(length(bytes)!=n*size)stop("cache numeric length")
                    v<-readBin(bytes,what=if(p$type=="double")"double" else "integer",n=n,size=size,endian="little")
                    if(p$type=="logical") {if(any(!is.na(v)&!v%in%c(0L,1L)))stop("cache logical");v<-as.logical(v)}
                    v
                })
            if(!is.null(p$attributes)) {
                if(!is.list(p$attributes)||is.null(names(p$attributes))||anyDuplicated(names(p$attributes))||
                   any(!names(p$attributes)%in%c("names","dim","dimnames")))stop("cache attributes")
                attrs<-lapply(p$attributes,unpack,depth=depth+1L)
                attributes(x)<-attrs
            }
            x
        }
        pl_fit_cache_read(unpack(wire$payload),key)
    },error=function(e)NULL)
}

pl_fit_cache_saved_text <- function(path) {
    # Read only our data-only text from the SDK's existing result protobuf.
    # In particular, never deserialize its state fields: the host discarded
    # those on purpose. Invalid, old or mismatching data is a normal cache miss.
    tryCatch({
        if(!is.character(path)||length(path)!=1L||!file.exists(path)||file.info(path)$size>64*1024^2)return("")
        core<-asNamespace("jmvcore");get("initProtoBuf",core)()
        con<-file(path,open="rb");on.exit(close(con))
        pb<-get("RProtoBuf_read",core)(get("jamovi.coms.AnalysisResponse",core),con)
        if(pb$ns!="pathlab"||pb$name!="paths"||!pb$results$has("group"))return("")
        for(item in pb$results$group$elements)if(item$name=="fitCache"&&item$has("preformatted"))
            return(if(nchar(item$preformatted,type="bytes")<=8*1024^2)item$preformatted else "")
        ""
    },error=function(e)"")
}

pl_fit_cache_saved <- function(path,key) {
    pl_fit_cache_from_json(pl_fit_cache_saved_text(path),key)
}
