# aggregate ====================================================================
#' @rdname aggregate
#' @name aggregate
NULL

#' @rdname aggregate
#' @export
setGeneric(
    name = "aggregateRows",
    def = function(object, ...) {
        standardGeneric("aggregateRows")
    }
)

#' @rdname aggregate
#' @export
setGeneric(
    name = "aggregateCols",
    def = function(object, ...) {
        standardGeneric("aggregateCols")
    }
)

#' @rdname aggregateCellsToSamples
#' @export
setGeneric(
    name = "aggregateCellsToSamples",
    def = function(object, ...) {
        standardGeneric("aggregateCellsToSamples")
    }
)



# cell2sample ==================================================================
#' @rdname cell2sample
#' @export
setGeneric(
    "cell2sample",
    function(object, ...) {
        standardGeneric("cell2sample")
    }
)



# collapseToString =============================================================
#' @rdname collapseToString
#' @export
setGeneric(
    name = "collapseToString",
    def = function(object, ...) {
        standardGeneric("collapseToString")
    }
)



# convertGenesToSymbols ========================================================
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



# convertTranscriptsToGenes ====================================================
#' @rdname convertTranscriptsToGenes
#' @export
setGeneric(
    name = "convertTranscriptsToGenes",
    def = function(object, ...) {
        standardGeneric("convertTranscriptsToGenes")
    }
)



# ensembl2entrez ===============================================================
#' @rdname ensembl2entrez
#' @export
setGeneric(
    name = "ensembl2entrez",
    def = function(object, ...) {
        standardGeneric("ensembl2entrez")
    }
)



# export =======================================================================
#' @rdname export
#' @export
setGeneric(
    name = "export",
    def = function(x, ...) {
        standardGeneric("export")
    }
)



# gene2symbol ==================================================================
#' @rdname gene2symbol
#' @export
setGeneric(
    name = "gene2symbol",
    def = function(object, ...) {
        standardGeneric("gene2symbol")
    }
)



# geometricMean ================================================================
#' @rdname geometricMean
#' @export
setGeneric(
    name = "geometricMean",
    def = function(object, ...) {
        standardGeneric("geometricMean")
    }
)



# interestingGroups ============================================================
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



# logRatio =====================================================================
#' @rdname logRatio
#' @name logRatio
NULL

#' @rdname logRatio
#' @export
setGeneric(
    name = "foldChangeToLogRatio",
    def = function(object, ...) {
        standardGeneric("foldChangeToLogRatio")
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



# makeNames ====================================================================
#' @rdname makeNames
#' @name makeNames
NULL

#' @rdname makeNames
#' @export
setGeneric(
    name = "camel",
    def = function(object, ...) {
        standardGeneric("camel")
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

#' @rdname makeNames
#' @export
setGeneric(
    name = "snake",
    def = function(object, ...) {
        standardGeneric("snake")
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



# mapGenes =====================================================================
#' @rdname mapGenes
#' @name mapGenes
NULL

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
    name = "mapGenesToIDs",
    def = function(object, ...) {
        standardGeneric("mapGenesToIDs")
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



# markdown =====================================================================
#' @rdname markdown
#' @export
setGeneric(
    name = "markdown",
    def = function(object, ...) {
        standardGeneric("markdown")
    }
)



# meltCounts ===================================================================
#' @rdname meltCounts
#' @export
setGeneric(
    name = "meltCounts",
    def = function(object, ...) {
        standardGeneric("meltCounts")
    }
)



# metrics ======================================================================
#' @rdname metrics
#' @export
setGeneric(
    name = "metrics",
    def = function(object, ...) {
        standardGeneric("metrics")
    }
)

#' @rdname metrics
#' @export
setGeneric(
    "metricsPerSample",
    function(object, ...) {
        standardGeneric("metricsPerSample")
    }
)



# plotCountsPerBiotype =========================================================
#' @rdname plotCountsPerBiotype
#' @export
setGeneric(
    name = "plotCountsPerBiotype",
    def = function(object, ...) {
        standardGeneric("plotCountsPerBiotype")
    }
)

#' @rdname plotCountsPerBiotype
#' @export
setGeneric(
    name = "plotCountsPerBroadClass",
    def = function(object, ...) {
        standardGeneric("plotCountsPerBroadClass")
    }
)



# plotCountsPerGene ============================================================
#' @rdname plotCountsPerGene
#' @export
setGeneric(
    name = "plotCountsPerGene",
    def = function(object, ...) {
        standardGeneric("plotCountsPerGene")
    }
)



# plotGenderMarkers ============================================================
#' @rdname plotGenderMarkers
#' @export
setGeneric(
    name = "plotGenderMarkers",
    def = function(object, ...) {
        standardGeneric("plotGenderMarkers")
    }
)



# plotGene =====================================================================
#' @rdname plotGene
#' @export
setGeneric(
    name = "plotGene",
    def = function(object, ...) {
        standardGeneric("plotGene")
    }
)



# plotGenesDetected ============================================================
#' @rdname plotGenesDetected
#' @export
setGeneric(
    name = "plotGenesDetected",
    def = function(object, ...) {
        standardGeneric("plotGenesDetected")
    }
)



# plotHeatmap ==================================================================
#' @rdname plotHeatmap
#' @name plotHeatmap
NULL

#' @rdname plotHeatmap
#' @export
setGeneric(
    name = "plotHeatmap",
    def = function(object, ...) {
        standardGeneric("plotHeatmap")
    }
)

#' @rdname plotHeatmap
#' @export
setGeneric(
    name = "plotCorrelationHeatmap",
    def = function(object, ...) {
        standardGeneric("plotCorrelationHeatmap")
    }
)

#' @rdname plotHeatmap
#' @export
setGeneric(
    name = "plotQuantileHeatmap",
    def = function(object, ...) {
        standardGeneric("plotQuantileHeatmap")
    }
)



# plotQC =======================================================================
#' @rdname plotQC
#' @export
setGeneric(
    name = "plotQC",
    def = function(object, ...) {
        standardGeneric("plotQC")
    }
)



# plotTotalCounts ==============================================================
#' @rdname plotTotalCounts
#' @export
setGeneric(
    name = "plotTotalCounts",
    def = function(object, ...) {
        standardGeneric("plotTotalCounts")
    }
)



# plotZerosVsDepth =============================================================
#' @rdname plotZerosVsDepth
#' @export
setGeneric(
    name = "plotZerosVsDepth",
    def = function(object, ...) {
        standardGeneric("plotZerosVsDepth")
    }
)

setGeneric(
    name = "zerosVsDepth",
    def = function(object, ...) {
        standardGeneric("zerosVsDepth")
    }
)



# removeNA =====================================================================
#' @rdname removeNA
#' @export
setGeneric(
    name = "removeNA",
    def = function(object, ...) {
        standardGeneric("removeNA")
    }
)



# sampleData ===================================================================
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



# sanitizeNA ===================================================================
#' @rdname sanitizeNA
#' @export
setGeneric(
    name = "sanitizeNA",
    def = function(object, ...) {
        standardGeneric("sanitizeNA")
    }
)



# sanitizePercent ==============================================================
#' @rdname sanitizePercent
#' @export
setGeneric(
    name = "sanitizePercent",
    def = function(object, ...) {
        standardGeneric("sanitizePercent")
    }
)



# selectSamples ================================================================
#' @rdname selectSamples
#' @export
setGeneric(
    name = "selectSamples",
    def = function(object, ...) {
        standardGeneric("selectSamples")
    }
)



# stripTranscriptVersions ======================================================
#' @rdname stripTranscriptVersions
#' @export
setGeneric(
    name = "stripTranscriptVersions",
    def = function(object, ...) {
        standardGeneric("stripTranscriptVersions")
    }
)



# subsetPerSample ==============================================================
#' @rdname subsetPerSample
#' @export
setGeneric(
    "subsetPerSample",
    function(object, ...) {
        standardGeneric("subsetPerSample")
    }
)



# topCellsPerSample ============================================================
#' @rdname topCellsPerSample
#' @export
setGeneric(
    "topCellsPerSample",
    function(object, ...) {
        standardGeneric("topCellsPerSample")
    }
)



# tx2gene ======================================================================
#' @rdname tx2gene
#' @export
setGeneric(
    name = "tx2gene",
    def = function(object, ...) {
        standardGeneric("tx2gene")
    }
)



# uniteInterestingGroups =======================================================
#' @rdname uniteInterestingGroups
#' @export
setGeneric(
    name = "uniteInterestingGroups",
    def = function(object, ...) {
        standardGeneric("uniteInterestingGroups")
    }
)
