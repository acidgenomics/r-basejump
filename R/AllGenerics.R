#' @rdname Ensembl2Entrez-class
#' @export
setGeneric(
    name = "Ensembl2Entrez",
    def = function(object, ...) {
        standardGeneric("Ensembl2Entrez")
    }
)



#' @rdname Gene2Symbol-class
#' @export
setGeneric(
    name = "Gene2Symbol",
    def = function(object, ...) {
        standardGeneric("Gene2Symbol")
    }
)



#' @rdname Tx2Gene-class
#' @export
setGeneric(
    name = "Tx2Gene",
    def = function(object, ...) {
        standardGeneric("Tx2Gene")
    }
)



#' @rdname aggregate
#' @name aggregateCols
#' @importFrom bioverbs aggregateCols
#' @usage aggregateCols(object, ...)
#' @export
NULL



#' @rdname aggregate
#' @name aggregateRows
#' @importFrom bioverbs aggregateRows
#' @usage aggregateRows(object, ...)
#' @export
NULL



#' @rdname aggregateCellsToSamples
#' @name aggregateCellsToSamples
#' @importFrom bioverbs aggregateCellsToSamples
#' @param ... Additional arguments.
#' @usage aggregateCellsToSamples(object, ...)
#' @export
NULL



#' @rdname autopadZeros
#' @name autopadZeros
#' @importFrom bioverbs autopadZeros
#' @param ... Additional arguments.
#' @usage autopadZeros(object, ...)
#' @export
NULL



#' @rdname cell2sample
#' @name cell2sample
#' @importFrom bioverbs cell2sample
#' @param ... Additional arguments.
#' @usage cell2sample(object, ...)
#' @export
NULL



#' @rdname collapseToString
#' @name collapseToString
#' @importFrom bioverbs collapseToString
#' @param ... Additional arguments.
#' @usage collapseToString(object, ...)
#' @export
NULL



#' @rdname combine
#' @name combine
#' @importFrom BiocGenerics combine
#' @param ... Additional arguments.
#' @usage combine(x, y, ...)
#' @export
NULL



#' @rdname convertGenesToSymbols
#' @name convertGenesToSymbols
#' @importFrom bioverbs convertGenesToSymbols
#' @param ... Additional arguments.
#' @usage convertGenesToSymbols(object, ...)
#' @export
NULL



#' @rdname convertSampleIDsToNames
#' @name convertSampleIDsToNames
#' @importFrom bioverbs convertSampleIDsToNames
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname convertGenesToSymbols
#' @name convertSymbolsToGenes
#' @importFrom bioverbs convertSymbolsToGenes
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname convertTranscriptsToGenes
#' @name convertTranscriptsToGenes
#' @importFrom bioverbs convertTranscriptsToGenes
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname counts
#' @name counts
#' @importFrom BiocGenerics counts
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname counts
#' @name counts<-
#' @importFrom BiocGenerics counts<-
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname geneNames
#' @name geneNames
#' @importFrom bioverbs geneNames
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname geometricMean
#' @name geometricMean
#' @importFrom bioverbs geometricMean
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname headtail
#' @name headtail
#' @importFrom bioverbs headtail
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname humanize
#' @name humanize
#' @importFrom bioverbs humanize
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname interestingGroups
#' @name interestingGroups
#' @importFrom bioverbs interestingGroups
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname interestingGroups
#' @name interestingGroups<-
#' @importFrom bioverbs interestingGroups<-
#' @param ... Additional arguments.
#' @export
NULL


#' @rdname logRatio
#' @name foldChangeToLogRatio
#' @importFrom bioverbs foldChangeToLogRatio
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname logRatio
#' @name logRatioToFoldChange
#' @importFrom bioverbs logRatioToFoldChange
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname mapGenes
#' @name mapGenesToIDs
#' @importFrom bioverbs mapGenesToIDs
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname mapGenes
#' @name mapGenesToRownames
#' @importFrom bioverbs mapGenesToRownames
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname mapGenes
#' @name mapGenesToSymbols
#' @importFrom bioverbs mapGenesToSymbols
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname markdown
#' @name markdown
#' @importFrom bioverbs markdown
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname mcolnames
#' @name mcolnames
#' @importFrom bioverbs mcolnames
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname mcolnames
#' @name mcolnames<-
#' @importFrom bioverbs mcolnames<-
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname meltCounts
#' @name meltCounts
#' @importFrom bioverbs meltCounts
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname metrics
#' @name metrics
#' @importFrom bioverbs metrics
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname metrics
#' @name metricsPerSample
#' @importFrom bioverbs metricsPerSample
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname organism
#' @name organism
#' @importFrom BiocGenerics organism
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname sampleData
#' @name sampleData
#' @importFrom bioverbs sampleData
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname sampleData
#' @name sampleData<-
#' @importFrom bioverbs sampleData<-
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname sampleNames
#' @name sampleNames
#' @importFrom Biobase sampleNames
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname sampleNames
#' @name sampleNames<-
#' @importFrom Biobase sampleNames<-
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname selectSamples
#' @name selectSamples
#' @importFrom bioverbs selectSamples
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname stripTranscriptVersions
#' @name stripTranscriptVersions
#' @importFrom bioverbs stripTranscriptVersions
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname subsetPerSample
#' @name subsetPerSample
#' @importFrom bioverbs subsetPerSample
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname topCellsPerSample
#' @name topCellsPerSample
#' @importFrom bioverbs topCellsPerSample
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname uniteInterestingGroups
#' @name uniteInterestingGroups
#' @importFrom bioverbs uniteInterestingGroups
#' @param ... Additional arguments.
#' @export
NULL



#' @rdname zerosVsDepth
#' @name zerosVsDepth
#' @importFrom bioverbs zerosVsDepth
#' @param ... Additional arguments.
#' @export
NULL
