#' @rdname aggregate
#' @export
setGeneric(
    name = "aggregateCols",
    def = function(object, ...) {
        standardGeneric("aggregateCols")
    }
)



#' @rdname aggregate
#' @export
setGeneric(
    name = "aggregateRows",
    def = function(object, ...) {
        standardGeneric("aggregateRows")
    }
)



#' @rdname broadClass
#' @export
setGeneric(
    name = "broadClass",
    def = function(object, ...) {
        standardGeneric("broadClass")
    }
)



#' @rdname makeNames
#' @export
setGeneric(
    name = "camel",
    def = function(object, ...) {
        standardGeneric("camel")
    }
)



#' @rdname cell2sample
#' @export
setGeneric(
    "cell2sample",
    function(object, ...) {
        standardGeneric("cell2sample")
    }
)



#' @rdname coerce
#' @export
setGeneric(
    name = "coerceS4ToList",
    def = function(from) {
        standardGeneric("coerceS4ToList")
    }
)



#' @rdname collapseToString
#' @export
setGeneric(
    name = "collapseToString",
    def = function(object, ...) {
        standardGeneric("collapseToString")
    }
)



#' @rdname convertGenesToSymbols
#' @export
setGeneric(
    name = "convertGenesToSymbols",
    def = function(object, ...) {
        standardGeneric("convertGenesToSymbols")
    }
)



#' @rdname convertGenesToSymbols
#' @export
setGeneric(
    name = "convertSymbolsToGenes",
    def = function(object, ...) {
        standardGeneric("convertSymbolsToGenes")
    }
)



#' @rdname convertTranscriptsToGenes
#' @export
setGeneric(
    name = "convertTranscriptsToGenes",
    def = function(object, ...) {
        standardGeneric("convertTranscriptsToGenes")
    }
)



#' @rdname makeNames
#' @export
setGeneric(
    name = "dotted",
    def = function(object, ...) {
        standardGeneric("dotted")
    }
)



#' @rdname ensembl2entrez
#' @export
setGeneric(
    name = "ensembl2entrez",
    def = function(object, ...) {
        standardGeneric("ensembl2entrez")
    }
)



#' @rdname export
#' @export
setGeneric(
    name = "export",
    def = function(x, ...) {
        standardGeneric("export")
    }
)



#' @rdname logRatio
#' @export
setGeneric(
    name = "foldChangeToLogRatio",
    def = function(object, ...) {
        standardGeneric("foldChangeToLogRatio")
    }
)



#' @rdname gene2symbol
#' @export
setGeneric(
    name = "gene2symbol",
    def = function(object, ...) {
        standardGeneric("gene2symbol")
    }
)



#' @rdname geometricMean
#' @export
setGeneric(
    name = "geometricMean",
    def = function(object, ...) {
        standardGeneric("geometricMean")
    }
)



#' @rdname interestingGroups
#' @export
setGeneric(
    name = "interestingGroups",
    def = function(object, ...) {
        standardGeneric("interestingGroups")
    }
)



#' @rdname interestingGroups
#' @export
setGeneric(
    name = "interestingGroups<-",
    def = function(object, ..., value) {
        standardGeneric("interestingGroups<-")
    }
)



#' @rdname logRatio
#' @export
setGeneric(
    name = "logRatioToFoldChange",
    def = function(object, ...) {
        standardGeneric("logRatioToFoldChange")
    }
)



#' @rdname mapGenes
#' @export
setGeneric(
    name = "mapGenesToIDs",
    def = function(object, ...) {
        standardGeneric("mapGenesToIDs")
    }
)



#' @rdname mapGenes
#' @export
setGeneric(
    name = "mapGenesToRownames",
    def = function(object, ...) {
        standardGeneric("mapGenesToRownames")
    }
)



#' @rdname mapGenes
#' @export
setGeneric(
    name = "mapGenesToSymbols",
    def = function(object, ...) {
        standardGeneric("mapGenesToSymbols")
    }
)



#' @rdname markdown
#' @export
setGeneric(
    name = "markdown",
    def = function(object, ...) {
        standardGeneric("markdown")
    }
)



#' @rdname meltCounts
#' @export
setGeneric(
    name = "meltCounts",
    def = function(object, ...) {
        standardGeneric("meltCounts")
    }
)



#' @rdname metrics
#' @export
setGeneric(
    name = "metrics",
    def = function(object, ...) {
        standardGeneric("metrics")
    }
)



#' @rdname metricsPerSample
#' @export
setGeneric(
    "metricsPerSample",
    function(object, ...) {
        standardGeneric("metricsPerSample")
    }
)



#' @rdname plotCorrelationHeatmap
#' @export
setGeneric(
    name = "plotCorrelationHeatmap",
    def = function(object, ...) {
        standardGeneric("plotCorrelationHeatmap")
    }
)



#' @rdname plotCountsPerBiotype
#' @export
setGeneric(
    name = "plotCountsPerBiotype",
    def = function(object, ...) {
        standardGeneric("plotCountsPerBiotype")
    }
)



#' @rdname plotCountsPerGene
#' @export
setGeneric(
    name = "plotCountsPerGene",
    def = function(object, ...) {
        standardGeneric("plotCountsPerGene")
    }
)



#' @rdname plotGenderMarkers
#' @export
setGeneric(
    name = "plotGenderMarkers",
    def = function(object, ...) {
        standardGeneric("plotGenderMarkers")
    }
)



#' @rdname plotGene
#' @export
setGeneric(
    name = "plotGene",
    def = function(object, ...) {
        standardGeneric("plotGene")
    }
)



#' @rdname plotGenesDetected
#' @export
setGeneric(
    name = "plotGenesDetected",
    def = function(object, ...) {
        standardGeneric("plotGenesDetected")
    }
)



#' @rdname plotHeatmap
#' @export
setGeneric(
    name = "plotHeatmap",
    def = function(object, ...) {
        standardGeneric("plotHeatmap")
    }
)



#' @rdname plotQuantileHeatmap
#' @export
setGeneric(
    name = "plotQuantileHeatmap",
    def = function(object, ...) {
        standardGeneric("plotQuantileHeatmap")
    }
)



#' @rdname plotQC
#' @export
setGeneric(
    name = "plotQC",
    def = function(object, ...) {
        standardGeneric("plotQC")
    }
)



#' @rdname plotTotalCounts
#' @export
setGeneric(
    name = "plotTotalCounts",
    def = function(object, ...) {
        standardGeneric("plotTotalCounts")
    }
)



#' @rdname plotZerosVsDepth
#' @export
setGeneric(
    name = "plotZerosVsDepth",
    def = function(object, ...) {
        standardGeneric("plotZerosVsDepth")
    }
)



#' @rdname removeNA
#' @export
setGeneric(
    name = "removeNA",
    def = function(object, ...) {
        standardGeneric("removeNA")
    }
)



#' @rdname sampleData
#' @export
setGeneric(
    name = "sampleData",
    def = function(object, ...) {
        standardGeneric("sampleData")
    }
)



#' @rdname sampleData
#' @export
setGeneric(
    name = "sampleData<-",
    def = function(object, ..., value) {
        standardGeneric("sampleData<-")
    }
)



#' @rdname sanitizeNA
#' @export
setGeneric(
    name = "sanitizeNA",
    def = function(object, ...) {
        standardGeneric("sanitizeNA")
    }
)



#' @rdname sanitizePercent
#' @export
setGeneric(
    name = "sanitizePercent",
    def = function(object, ...) {
        standardGeneric("sanitizePercent")
    }
)



#' @rdname selectSamples
#' @export
setGeneric(
    name = "selectSamples",
    def = function(object, ...) {
        standardGeneric("selectSamples")
    }
)



#' @rdname makeNames
#' @export
setGeneric(
    name = "snake",
    def = function(object, ...) {
        standardGeneric("snake")
    }
)



#' @rdname stripTranscriptVersions
#' @export
setGeneric(
    name = "stripTranscriptVersions",
    def = function(object, ...) {
        standardGeneric("stripTranscriptVersions")
    }
)



#' @rdname subsetPerSample
#' @export
setGeneric(
    "subsetPerSample",
    function(object, ...) {
        standardGeneric("subsetPerSample")
    }
)



#' @rdname topCells
#' @export
setGeneric(
    "topCells",
    function(object, ...) {
        standardGeneric("topCells")
    }
)



#' @rdname tx2gene
#' @export
setGeneric(
    name = "tx2gene",
    def = function(object, ...) {
        standardGeneric("tx2gene")
    }
)



#' @rdname uniteInterestingGroups
#' @export
setGeneric(
    name = "uniteInterestingGroups",
    def = function(object, ...) {
        standardGeneric("uniteInterestingGroups")
    }
)



#' @rdname makeNames
#' @export
setGeneric(
    name = "upperCamel",
    def = function(object, ...) {
        standardGeneric("upperCamel")
    }
)



setGeneric(
    name = "zerosVsDepth",
    def = function(object, ...) {
        standardGeneric("zerosVsDepth")
    }
)
