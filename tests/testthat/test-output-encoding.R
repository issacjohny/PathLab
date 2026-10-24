pl_output_module_dir <- function() {
    root<-getOption("pathlab.root")
    if(!is.null(root))return(file.path(root,"modules/pathlab"))
    # R CMD check installs the R lazy-load database, while retaining the actual
    # source archive under 00_pkg_src. Inspect its bytes, not installed stubs.
    candidates<-c(testthat::test_path("..",".."),testthat::test_path("..","..","00_pkg_src","pathlab"))
    found<-candidates[file.exists(file.path(candidates,"R/output.R"))]
    if(!length(found))stop("Raw package sources are required for the source-encoding checks")
    found[[1L]]
}
pl_output_spec <- function() {
    list(nodes=list(
      list(id="x",column="<script>Exposure</script> & \"A\"",role="independent"),
      list(id="m1",column="M\u00e9diator",role="mediator"),
      list(id="y",column="Outcome",role="dependent"),
      list(id="w",column="Moderator",role="moderator")),
      edges=list(list(id="a",source="x",target="m1"),list(id="b",source="m1",target="y"),list(id="c",source="x",target="y"),list(id="d",source="w",target="m1")),
      moderations=list(list(id="aw",path_id="a",moderator_ids=list("w"))))
}

testthat::test_that("native R sources use portable ASCII source bytes",{
    files<-list.files(file.path(pl_output_module_dir(),"R"),pattern="[.]R$",full.names=TRUE)
    testthat::expect_gt(length(files),0)
    for(file in files) {
        bytes<-readBin(file,"raw",n=file.info(file)$size)
        testthat::expect_true(all(as.integer(bytes)<128L),info=basename(file))
    }
})

testthat::test_that("source encoding cannot corrupt native path and interaction symbols",{
    # R package preparation can source functions using a Windows native
    # encoding. ASCII Unicode escapes retain the same characters in that path.
    env<-new.env(parent=environment())
    source(file.path(pl_output_module_dir(),"R/output.R"),local=env,encoding="latin1")
    spec<-pl_output_spec()
    term<-env$pl_term_label(spec,"w:x")
    path<-env$pl_path_label(spec,c("a","b"))
    testthat::expect_identical(term,paste("Moderator","\u00d7",spec$nodes[[1]]$column))
    testthat::expect_identical(path,paste(vapply(spec$nodes[1:3],function(n)n$column,""),collapse=" \u2192 "))
    testthat::expect_true(validUTF8(term))
    testthat::expect_true(validUTF8(path))
    testthat::expect_false(grepl("\u00e2\u0086|\u00c3\u0097",path,perl=TRUE))
    testthat::expect_identical(env$pl_term_label(spec,"(Intercept)"),"Intercept")
    testthat::expect_identical(env$pl_condition_label(spec,list(w=.5)),"Moderator = 0.5")
})

testthat::test_that("native SVG preserves Unicode labels and escapes HTML text",{
    spec<-pl_output_spec();svg<-pl_diagram(spec)
    testthat::expect_match(svg,"&lt;script&gt;Exposure&lt;/script&gt; &amp; &quot;A&quot;",fixed=TRUE)
    testthat::expect_false(grepl("<script>",svg,fixed=TRUE))
    testthat::expect_match(svg,"M\u00e9diator",fixed=TRUE)
    testthat::expect_true(validUTF8(svg))
    testthat::expect_equal(length(gregexpr("<rect ",svg,fixed=TRUE)[[1]]),length(spec$nodes))
    testthat::expect_match(svg,'stroke-dasharray="5,4"',fixed=TRUE)
    testthat::expect_match(svg,'role="img"',fixed=TRUE)
    testthat::expect_identical(pl_html_escape("<&>\"'"),"&lt;&amp;&gt;&quot;&#39;")
})

testthat::test_that("diagram retains every covariate and declared equation-specific arrow",{
    spec<-pl_output_spec()
    spec$nodes<-c(spec$nodes,list(list(id="c1",column="Age & <range>",role="covariate"),list(id="c2",column="Baseline",role="covariate")))
    spec$edges<-c(spec$edges,list(list(id="c1_m1",source="c1",target="m1"),list(id="c1_y",source="c1",target="y"),list(id="c2_y",source="c2",target="y")))
    layout<-pl_diagram_layout(spec);svg<-pl_diagram(spec)
    testthat::expect_setequal(names(layout$positions),pl_ids(spec$nodes))
    testthat::expect_setequal(names(layout$edges),pl_ids(spec$edges))
    testthat::expect_gt(layout$height,260)
    testthat::expect_equal(length(gregexpr('data-node-id="',svg,fixed=TRUE)[[1]]),length(spec$nodes))
    testthat::expect_equal(length(gregexpr('data-path-id="',svg,fixed=TRUE)[[1]]),length(spec$edges))
    for(e in spec$edges)testthat::expect_match(svg,paste0('data-path-id="',e$id,'"'),fixed=TRUE)
    testthat::expect_match(svg,"Age &amp; &lt;range&gt;",fixed=TRUE)
    for(id in c("c1_m1","c1_y","c2_y")) {
        e<-Filter(function(e)e$id==id,spec$edges)[[1]]
        geometry<-layout$edges[[id]]
        testthat::expect_lt(geometry$from[2],layout$positions[[e$source]][2])
        testthat::expect_gt(geometry$to[2],layout$positions[[e$target]][2])
        testthat::expect_gt(geometry$from[2],geometry$to[2])
    }
    # A moderation attachment follows the drawn curve, rather than the
    # straight centre-to-centre chord for that same declared path.
    spec$moderations[[1]]$path_id<-"c"
    curved<-pl_diagram_layout(spec)$edges$c
    testthat::expect_false(isTRUE(all.equal(curved$midpoint,(layout$positions$x+layout$positions$y)/2)))
    testthat::expect_match(pl_diagram(spec),'data-moderation-id="aw"',fixed=TRUE)
})
