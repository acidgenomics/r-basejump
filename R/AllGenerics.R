#' S4 Generics
#'
#' @name AllGenerics
#' @keywords internal
#'
#' @inheritParams general
#'
#' @return Varies, depending upon the method.
NULL



#' @rdname aggregate
#' @export
setGeneric(
    name = "aggregateFeatures",
    def = function(object, ...) {
        standardGeneric("aggregateFeatures")
    }
)



#' @rdname aggregate
#' @export
setGeneric(
    "aggregateReplicates",
    function(object, ...) {
        standardGeneric("aggregateReplicates")
    }
)



#' @rdname broadClass
#' @export
setGeneric("broadClass", function(object, ...) {
    standardGeneric("broadClass")
})



#' @rdname makeNames
#' @export
setGeneric(
    "camel",
    function(object, ...) {
        standardGeneric("camel")
    }
)



#' @rdname collapseToString
#' @export
setGeneric(
    "collapseToString",
    function(object, ...) {
        standardGeneric("collapseToString")
    }
)



#' @rdname convertGenesToSymbols
#' @export
setGeneric(
    "convertGenesToSymbols",
    function(object, ...) {
        standardGeneric("convertGenesToSymbols")
    }
)



#' @rdname convertGenesToSymbols
#' @export
setGeneric(
    "convertSymbolsToGenes",
    function(object, ...) {
        standardGeneric("convertSymbolsToGenes")
    }
)



#' @rdname convertTranscriptsToGenes
#' @export
setGeneric(
    "convertTranscriptsToGenes",
    function(object, ...) {
        standardGeneric("convertTranscriptsToGenes")
    }
)



#' @rdname detectOrganism
#' @export
setGeneric(
    "detectOrganism",
    function(object, ...) {
        standardGeneric("detectOrganism")
    }
)



#' @rdname makeNames
#' @export
setGeneric(
    "dotted",
    function(object, ...) {
        standardGeneric("dotted")
    }
)



#' @rdname logRatio
#' @export
setGeneric(
    "foldChangeToLogRatio",
    function(object, ...) {
        standardGeneric("foldChangeToLogRatio")
    }
)



#' @rdname fixNA
#' @export
setGeneric(
    "fixNA",
    function(object, ...) {
        standardGeneric("fixNA")
    }
)



#' @rdname flatFiles
#' @export
setGeneric(
    "flatFiles",
    function(object, ...) {
        standardGeneric("flatFiles")
    }
)



#' @rdname gene2symbol
#' @export
setGeneric(
    "gene2symbol",
    function(object, ...) {
        standardGeneric("gene2symbol")
    }
)



#' @rdname geometricMean
#' @export
setGeneric(
    "geometricMean",
    function(object, ...) {
        standardGeneric("geometricMean")
    }
)



#' @rdname interestingGroups
#' @export
setGeneric(
    "interestingGroups",
    function(object, ...) {
        standardGeneric("interestingGroups")
    }
)



#' @rdname interestingGroups
#' @export
setGeneric(
    "interestingGroups<-",
    function(object, ..., value) {
        standardGeneric("interestingGroups<-")
    }
)



#' @rdname logRatio
#' @export
setGeneric(
    "logRatioToFoldChange",
    function(object, ...) {
        standardGeneric("logRatioToFoldChange")
    }
)



#' @rdname mapGenes
#' @export
setGeneric(
    "mapGenesToIDs",
    function(object, ...) {
        standardGeneric("mapGenesToIDs")
    }
)



#' @rdname mapGenes
#' @export
setGeneric(
    "mapGenesToRownames",
    function(object, ...) {
        standardGeneric("mapGenesToRownames")
    }
)



#' @rdname mapGenes
#' @export
setGeneric(
    "mapGenesToSymbols",
    function(object, ...) {
        standardGeneric("mapGenesToSymbols")
    }
)



#' @rdname AllGenerics
#' @export
setGeneric(
    "metrics",
    function(object, ...) {
        standardGeneric("metrics")
    }
)



#' @rdname plotCorrelationHeatmap
#' @export
setGeneric(
    "plotCorrelationHeatmap",
    function(object, ...) {
        standardGeneric("plotCorrelationHeatmap")
    }
)



#' @rdname AllGenerics
#' @export
setGeneric(
    "plotGene",
    function(object, ...) {
        standardGeneric("plotGene")
    }
)



#' @rdname plotHeatmap
#' @export
setGeneric(
    "plotHeatmap",
    function(object, ...) {
        standardGeneric("plotHeatmap")
    }
)



#' @rdname plotQuantileHeatmap
#' @export
setGeneric(
    "plotQuantileHeatmap",
    function(object, ...) {
        standardGeneric("plotQuantileHeatmap")
    }
)



#' @rdname AllGenerics
#' @export
setGeneric(
    "plotQC",
    function(object, ...) {
        standardGeneric("plotQC")  # nocov
    }
)



#' @rdname removeNA
#' @export
setGeneric(
    "removeNA",
    function(object, ...) {
        standardGeneric("removeNA")
    }
)



#' @rdname sampleData
#' @export
setGeneric(
    "sampleData",
    function(object, ...) {
        standardGeneric("sampleData")
    }
)



#' @rdname sampleData
#' @export
setGeneric(
    "sampleData<-",
    function(object, ..., value) {
        standardGeneric("sampleData<-")
    }
)



#' @rdname selectSamples
#' @export
setGeneric(
    "selectSamples",
    function(object, ...) {
        standardGeneric("selectSamples")
    }
)



#' @rdname makeNames
#' @export
setGeneric(
    "snake",
    function(object, ...) {
        standardGeneric("snake")
    }
)



#' @rdname stripTranscriptVersions
#' @export
setGeneric(
    "stripTranscriptVersions",
    function(object, ...) {
        standardGeneric("stripTranscriptVersions")
    }
)



#' @rdname uniteInterestingGroups
#' @export
setGeneric(
    "uniteInterestingGroups",
    function(object, ...) {
        standardGeneric("uniteInterestingGroups")
    }
)



#' @rdname makeNames
#' @export
setGeneric(
    "upperCamel",
    function(object, ...) {
        standardGeneric("upperCamel")
    }
)
