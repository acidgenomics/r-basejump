#' S4 Generics
#'
#' @rdname AllGenerics
#' @name AllGenerics
#' @keywords internal
#'
#' @param object Object.
#' @param x Primary object.
#' @param y Secondary object.
#' @param value Value to assign.
#' @param ... *Additional arguments (for the S4 generic definition).*
NULL



#' @rdname aggregateFeatures
#' @export
setGeneric("aggregateFeatures", function(object, ...) {
    standardGeneric("aggregateFeatures")
})



#' @rdname aggregateReplicates
#' @export
setGeneric("aggregateReplicates", function(object, ...) {
    standardGeneric("aggregateReplicates")
})



#' @rdname annotable
#' @export
setGeneric("annotable", function(object, ...) {
    standardGeneric("annotable")
})



#' @rdname camel
#' @export
setGeneric("camel", function(object, ...) {
    standardGeneric("camel")
})



#' @rdname collapseToString
#' @export
setGeneric("collapseToString", function(object, ...) {
    standardGeneric("collapseToString")
})



#' @rdname convertGenesToSymbols
#' @export
setGeneric("convertGenesToSymbols", function(object, ...) {
    standardGeneric("convertGenesToSymbols")
})



#' @rdname convertTranscriptsToGenes
#' @export
setGeneric("convertTranscriptsToGenes", function(object, ...) {
    standardGeneric("convertTranscriptsToGenes")
})



#' @rdname detectOrganism
#' @export
setGeneric("detectOrganism", function(object, ...) {
    standardGeneric("detectOrganism")
})



#' @rdname dots
#' @export
setGeneric(
    "dots",
    signature = "...",
    function(..., character = FALSE) {
    standardGeneric("dots")
})



#' @rdname dotted
#' @export
setGeneric("dotted", function(object, ...) {
    standardGeneric("dotted")
})



#' @rdname dynamicPlotlist
#' @export
setGeneric("dynamicPlotlist", function(object, ...) {
    standardGeneric("dynamicPlotlist")
})



#' @rdname logRatio
#' @export
setGeneric("foldChangeToLogRatio", function(object, ...) {
    standardGeneric("foldChangeToLogRatio")
})



#' @rdname fixNA
#' @export
setGeneric("fixNA", function(object, ...) {
    standardGeneric("fixNA")
})



#' @rdname gene2symbol
#' @export
setGeneric("gene2symbol", function(object, ...) {
    standardGeneric("gene2symbol")
})



#' @rdname gene2symbolFromGFF
#' @export
setGeneric("gene2symbolFromGFF", function(object, ...) {
    standardGeneric("gene2symbolFromGFF")
})



#' @rdname geometricMean
setGeneric("geometricMean", function(object, ...) {
    standardGeneric("geometricMean")
})



#' @rdname grepString
setGeneric("grepString", function(object, ...) {
    standardGeneric("grepString")
})



#' @rdname kables
#' @export
setGeneric("kables", function(object, ...) {
    standardGeneric("kables")
})



#' @rdname logRatio
#' @export
setGeneric("logRatioToFoldChange", function(object, ...) {
    standardGeneric("logRatioToFoldChange")
})



#' @rdname markdownHeader
#' @export
setGeneric("markdownHeader", function(object, ...) {
    standardGeneric("markdownHeader")
})



#' @rdname markdownList
#' @export
setGeneric("markdownList", function(object, ...) {
    standardGeneric("markdownList")
})



#' @rdname markdownPlotlist
#' @export
setGeneric("markdownPlotlist", function(object, ...) {
    standardGeneric("markdownPlotlist")
})



#' @rdname plotHeatmap
#' @export
setGeneric("plotHeatmap", function(object, ...) {
    standardGeneric("plotHeatmap")
})



#' @rdname plotQuantileHeatmap
#' @export
setGeneric("plotQuantileHeatmap", function(object, ...) {
    standardGeneric("plotQuantileHeatmap")
})



#' @rdname readFileByExtension
#' @export
setGeneric("readFileByExtension", function(object, ...) {
    standardGeneric("readFileByExtension")
})



#' @rdname readGFF
#' @export
setGeneric("readGFF", function(object, ...) {
    standardGeneric("readGFF")
})



#' @rdname readYAML
#' @export
setGeneric("readYAML", function(object, ...) {
    standardGeneric("readYAML")
})



#' @rdname removeNA
#' @export
setGeneric("removeNA", function(object, ...) {
    standardGeneric("removeNA")
})



#' @rdname snake
#' @export
setGeneric("snake", function(object, ...) {
    standardGeneric("snake")
})



#' @rdname sortUnique
#' @export
setGeneric("sortUnique", function(object, ...) {
    standardGeneric("sortUnique")
})



#' @rdname stripTranscriptVersions
#' @export
setGeneric("stripTranscriptVersions", function(object, ...) {
    standardGeneric("stripTranscriptVersions")
})



#' @rdname toStringUnique
#' @export
setGeneric("toStringUnique", function(object, ...) {
    standardGeneric("toStringUnique")
})



#' @rdname tx2gene
#' @export
setGeneric("tx2gene", function(object, ...) {
    standardGeneric("tx2gene")
})



#' @rdname tx2geneFromGFF
#' @export
setGeneric("tx2geneFromGFF", function(object, ...) {
    standardGeneric("tx2geneFromGFF")
})



#' @rdname camel
#' @export
setGeneric("upperCamel", function(object, ...) {
    standardGeneric("upperCamel")
})
