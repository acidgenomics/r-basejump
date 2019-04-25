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
#' @usage aggregateCellsToSamples(object, ...)
#' @export
NULL

#' @rdname autopadZeros
#' @name autopadZeros
#' @importFrom bioverbs autopadZeros
#' @usage autopadZeros(object, ...)
#' @export
NULL

#' @rdname cell2sample
#' @name cell2sample
#' @importFrom bioverbs cell2sample
#' @usage cell2sample(object, ...)
#' @export
NULL

#' @rdname collapseToString
#' @name collapseToString
#' @importFrom bioverbs collapseToString
#' @usage collapseToString(object, ...)
#' @export
NULL

#' @rdname combine
#' @name combine
#' @importFrom BiocGenerics combine
#' @usage combine(x, y, ...)
#' @export
NULL

#' @rdname convertGenesToSymbols
#' @name convertGenesToSymbols
#' @importFrom bioverbs convertGenesToSymbols
#' @usage convertGenesToSymbols(object, ...)
#' @export
NULL

#' @rdname convertSampleIDsToNames
#' @name convertSampleIDsToNames
#' @importFrom bioverbs convertSampleIDsToNames
#' @usage convertSampleIDsToNames(object, ...)
#' @export
NULL

#' @rdname convertGenesToSymbols
#' @name convertSymbolsToGenes
#' @importFrom bioverbs convertSymbolsToGenes
#' @usage convertSymbolsToGenes(object, ...)
#' @export
NULL

#' @rdname convertTranscriptsToGenes
#' @name convertTranscriptsToGenes
#' @importFrom bioverbs convertTranscriptsToGenes
#' @usage convertTranscriptsToGenes(object, ...)
#' @export
NULL

#' @rdname counts
#' @name counts
#' @importFrom BiocGenerics counts
#' @usage counts(object, ...)
#' @export
NULL

#' @rdname counts
#' @name counts<-
#' @importFrom BiocGenerics counts<-
#' @usage counts(object, ...) <- value
#' @export
NULL

#' @rdname geneNames
#' @name geneNames
#' @importFrom bioverbs geneNames
#' @usage geneNames(object, ...)
#' @export
NULL

#' @rdname geometricMean
#' @name geometricMean
#' @importFrom bioverbs geometricMean
#' @usage geometricMean(object, ...)
#' @export
NULL

#' @rdname headtail
#' @name headtail
#' @importFrom bioverbs headtail
#' @usage headtail(x, ...)
#' @export
NULL

#' @rdname humanize
#' @name humanize
#' @importFrom bioverbs humanize
#' @usage humanize(object, ...)
#' @export
NULL

#' @rdname interestingGroups
#' @name interestingGroups
#' @importFrom bioverbs interestingGroups
#' @usage interestingGroups(object, ...)
#' @export
NULL

#' @rdname interestingGroups
#' @name interestingGroups<-
#' @importFrom bioverbs interestingGroups<-
#' @usage interestingGroups(object, ...)  <- value
#' @export
NULL

#' @rdname logRatio
#' @name foldChangeToLogRatio
#' @importFrom bioverbs foldChangeToLogRatio
#' @usage foldChangeToLogRatio(object, ...)
#' @export
NULL

#' @rdname logRatio
#' @name logRatioToFoldChange
#' @importFrom bioverbs logRatioToFoldChange
#' @usage logRatioToFoldChange(object, ...)
#' @export
NULL

#' @rdname mapGenes
#' @name mapGenesToIDs
#' @importFrom bioverbs mapGenesToIDs
#' @usage mapGenesToIDs(object, ...)
#' @export
NULL

#' @rdname mapGenes
#' @name mapGenesToRownames
#' @importFrom bioverbs mapGenesToRownames
#' @usage mapGenesToRownames(object, ...)
#' @export
NULL

#' @rdname mapGenes
#' @name mapGenesToSymbols
#' @importFrom bioverbs mapGenesToSymbols
#' @usage mapGenesToSymbols(object, ...)
#' @export
NULL

#' @rdname markdown
#' @name markdown
#' @importFrom bioverbs markdown
#' @usage markdown(object, ...)
#' @export
NULL

#' @rdname mcolnames
#' @name mcolnames
#' @importFrom bioverbs mcolnames
#' @usage mcolnames(x, ...)
#' @export
NULL

#' @rdname mcolnames
#' @name mcolnames<-
#' @importFrom bioverbs mcolnames<-
#' @usage mcolnames(x, ...) <- value
#' @export
NULL

#' @rdname meltCounts
#' @name meltCounts
#' @importFrom bioverbs meltCounts
#' @usage meltCounts(object, ...)
#' @export
NULL

#' @rdname metrics
#' @name metrics
#' @importFrom bioverbs metrics
#' @usage metrics(object, ...)
#' @export
NULL

#' @rdname metrics
#' @name metricsPerSample
#' @importFrom bioverbs metricsPerSample
#' @usage metricsPerSample(object, ...)
#' @export
NULL

#' @rdname organism
#' @name organism
#' @importFrom BiocGenerics organism
#' @usage organism(object)
#' @export
NULL

#' @rdname sampleData
#' @name sampleData
#' @importFrom bioverbs sampleData
#' @usage sampleData(object, ...)
#' @export
NULL

#' @rdname sampleData
#' @name sampleData<-
#' @importFrom bioverbs sampleData<-
#' @usage sampleData(object, ...) <- value
#' @export
NULL



#' @rdname sampleNames
#' @name sampleNames
#' @importFrom Biobase sampleNames
#' @usage sampleNames(object)
#' @export
NULL



#' @rdname sampleNames
#' @name sampleNames<-
#' @importFrom Biobase sampleNames<-
#' @usage sampleNames(object) <- value
#' @export
NULL



#' @rdname selectSamples
#' @name selectSamples
#' @importFrom bioverbs selectSamples
#' @usage selectSamples(object, ...)
#' @export
NULL



#' @rdname stripTranscriptVersions
#' @name stripTranscriptVersions
#' @importFrom bioverbs stripTranscriptVersions
#' @usage stripTranscriptVersions(object, ...)
#' @export
NULL



#' @rdname subsetPerSample
#' @name subsetPerSample
#' @importFrom bioverbs subsetPerSample
#' @usage subsetPerSample(object, ...)
#' @export
NULL



#' @rdname topCellsPerSample
#' @name topCellsPerSample
#' @importFrom bioverbs topCellsPerSample
#' @usage topCellsPerSample(object, ...)
#' @export
NULL



#' @rdname uniteInterestingGroups
#' @name uniteInterestingGroups
#' @importFrom bioverbs uniteInterestingGroups
#' @usage uniteInterestingGroups(object, ...)
#' @export
NULL



#' @rdname zerosVsDepth
#' @name zerosVsDepth
#' @importFrom bioverbs zerosVsDepth
#' @usage zerosVsDepth(object, ...)
#' @export
NULL
