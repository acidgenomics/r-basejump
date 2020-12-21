## FIXME NEED TO EXPORT IN ACIDGENERICS.

#' @rdname aggregate
#' @name aggregate
#' @importFrom S4Vectors aggregate
#' @usage aggregate(x, ...)
#' @export
NULL



#' @rdname aggregateCellsToSamples
#' @name aggregateCellsToSamples
#' @importFrom AcidGenerics aggregateCellsToSamples
#' @usage aggregateCellsToSamples(x, ...)
#' @export
NULL



#' @rdname aggregateCols
#' @name aggregateCols
#' @importFrom AcidGenerics aggregateCols
#' @usage aggregateCols(x, ...)
#' @export
NULL



#' @rdname aggregateRows
#' @name aggregateRows
#' @importFrom AcidGenerics aggregateRows
#' @usage aggregateRows(x, ...)
#' @export
NULL



#' @rdname autopadZeros
#' @name autopadZeros
#' @importFrom AcidGenerics autopadZeros
#' @importMethodsFrom syntactic autopadZeros
#' @usage autopadZeros(object, ...)
#' @export
NULL



#' @rdname calculateMetrics
#' @name calculateMetrics
#' @importFrom AcidGenerics calculateMetrics
#' @usage calculateMetrics(object, ...)
#' @export
NULL



#' @rdname camelCase
#' @name camelCase
#' @importFrom syntactic camelCase
#' @usage camelCase(object, ...)
#' @export
NULL



#' @rdname cell2sample
#' @name cell2sample
#' @importFrom AcidGenerics cell2sample
#' @usage cell2sample(object, ...)
#' @export
NULL



#' @rdname collapseToString
#' @name collapseToString
#' @importFrom AcidGenerics collapseToString
#' @usage collapseToString(object, ...)
#' @export
NULL



#' @rdname combine
#' @name combine
#' @importFrom AcidGenerics combine
#' @usage combine(x, y, ...)
#' @export
NULL



#' @rdname convertGenesToSymbols
#' @name convertGenesToSymbols
#' @importFrom AcidGenerics convertGenesToSymbols
#' @usage convertGenesToSymbols(object, ...)
#' @export
NULL

#' @rdname convertGenesToSymbols
#' @name convertSymbolsToGenes
#' @importFrom AcidGenerics convertSymbolsToGenes
#' @usage convertSymbolsToGenes(object, ...)
#' @export
NULL



#' @rdname convertSampleIDsToNames
#' @name convertSampleIDsToNames
#' @importFrom AcidGenerics convertSampleIDsToNames
#' @usage convertSampleIDsToNames(object, ...)
#' @export
NULL



#' @rdname convertTranscriptsToGenes
#' @name convertTranscriptsToGenes
#' @importFrom AcidGenerics convertTranscriptsToGenes
#' @usage convertTranscriptsToGenes(object, ...)
#' @export
NULL



#' @rdname correlation
#' @name correlation
#' @importFrom AcidGenerics correlation
#' @usage correlation(x, y, ...)
#' @export
NULL



#' @rdname counts
#' @name counts
#' @importFrom AcidGenerics counts
#' @usage counts(object, ...)
#' @export
NULL

#' @rdname counts
#' @name counts<-
#' @importFrom AcidGenerics counts<-
#' @usage counts(object, ...) <- value
#' @export
NULL



#' @rdname dottedCase
#' @name dottedCase
#' @importFrom syntactic dottedCase
#' @usage dottedCase(object, ...)
#' @export
NULL



#' @rdname estimateSizeFactors
#' @name estimateSizeFactors
#' @importFrom AcidGenerics estimateSizeFactors
#' @usage estimateSizeFactors(object, ...)
#' @export
NULL



#' @rdname filterCells
#' @name filterCells
#' @importFrom AcidGenerics filterCells
#' @usage filterCells(object, ...)
#' @export
NULL



#' @rdname geometricMean
#' @name geometricMean
#' @importFrom AcidGenerics geometricMean
#' @usage geometricMean(object, ...)
#' @export
NULL



#' @rdname headtail
#' @name headtail
#' @importFrom AcidGenerics headtail
#' @usage headtail(x, ...)
#' @export
NULL



#' @rdname humanize
#' @name humanize
#' @importFrom AcidGenerics humanize
#' @usage humanize(object, ...)
#' @export
NULL



#' @rdname integerCounts
#' @export
setGeneric(
    name = "integerCounts",
    def = function(object, ...) {
        standardGeneric("integerCounts")
    }
)



#' @rdname interestingGroups
#' @name interestingGroups
#' @importFrom AcidGenerics interestingGroups
#' @usage interestingGroups(object, ...)
#' @export
NULL

#' @rdname interestingGroups
#' @name interestingGroups<-
#' @importFrom AcidGenerics interestingGroups<-
#' @usage interestingGroups(object, ...)  <- value
#' @export
NULL



#' @rdname intersectAll
#' @name intersectAll
#' @importFrom AcidGenerics intersectAll
#' @usage intersectAll(object, ...)
#' @export
NULL



#' @rdname intersectionMatrix
#' @name intersectionMatrix
#' @importFrom AcidGenerics intersectionMatrix
#' @usage intersectionMatrix(object, ...)
#' @export
NULL



#' @rdname logRatio
#' @name foldChangeToLogRatio
#' @importFrom AcidGenerics foldChangeToLogRatio
#' @usage foldChangeToLogRatio(object, ...)
#' @export
NULL

#' @rdname logRatio
#' @name logRatioToFoldChange
#' @importFrom AcidGenerics logRatioToFoldChange
#' @usage logRatioToFoldChange(object, ...)
#' @export
NULL



#' @rdname makeSampleData
#' @export
setGeneric(
    name = "makeSampleData",
    def = function(object, ...) {
        standardGeneric("makeSampleData")
    }
)



#' @rdname mapGenes
#' @name mapGenesToRownames
#' @importFrom AcidGenerics mapGenesToRownames
#' @usage mapGenesToRownames(object, ...)
#' @export
NULL

#' @rdname mapGenes
#' @name mapGenesToIDs
#' @importFrom AcidGenerics mapGenesToIDs
#' @usage mapGenesToIDs(object, ...)
#' @export
NULL

#' @rdname mapGenes
#' @name mapGenesToSymbols
#' @importFrom AcidGenerics mapGenesToSymbols
#' @usage mapGenesToSymbols(object, ...)
#' @export
NULL



#' @rdname markdown
#' @name markdown
#' @importFrom AcidGenerics markdown
#' @usage markdown(object, ...)
#' @export
NULL



#' @rdname mcolnames
#' @name mcolnames
#' @importFrom AcidGenerics mcolnames
#' @usage mcolnames(x, ...)
#' @export
NULL

#' @rdname mcolnames
#' @name mcolnames<-
#' @importFrom AcidGenerics mcolnames<-
#' @usage mcolnames(x, ...) <- value
#' @export
NULL



#' @rdname melt
#' @name melt
#' @importFrom AcidGenerics melt
#' @usage melt(object, ...)
#' @export
NULL



#' @rdname metrics
#' @name metrics
#' @importFrom AcidGenerics metrics
#' @usage metrics(object, ...)
#' @export
NULL

#' @rdname metrics
#' @name metricsPerSample
#' @importFrom AcidGenerics metricsPerSample
#' @usage metricsPerSample(object, ...)
#' @export
NULL



#' @rdname nonzeroRowsAndCols
#' @name nonzeroRowsAndCols
#' @importFrom AcidGenerics nonzeroRowsAndCols
#' @usage nonzeroRowsAndCols(object, ...)
#' @export
NULL



#' @rdname sampleData
#' @name sampleData
#' @importFrom AcidGenerics sampleData
#' @usage sampleData(object, ...)
#' @export
NULL

#' @rdname sampleData
#' @name sampleData<-
#' @importFrom AcidGenerics sampleData<-
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
#' @importFrom AcidGenerics selectSamples
#' @usage selectSamples(object, ...)
#' @export
NULL



#' @rdname sem
#' @name sem
#' @importFrom AcidGenerics sem
#' @usage sem(x, ...)
#' @export
NULL



#' @rdname sizeFactors
#' @name sizeFactors
#' @importFrom AcidGenerics sizeFactors
#' @usage sizeFactors(object, ...)
#' @export
NULL

#' @rdname sizeFactors
#' @name sizeFactors<-
#' @importFrom AcidGenerics sizeFactors<-
#' @usage sizeFactors(object, ...) <- value
#' @export
NULL



#' @rdname snakeCase
#' @name snakeCase
#' @importFrom syntactic snakeCase
#' @usage snakeCase(object, ...)
#' @export
NULL



#' @rdname subsetPerSample
#' @name subsetPerSample
#' @importFrom AcidGenerics subsetPerSample
#' @usage subsetPerSample(object, ...)
#' @export
NULL



#' @rdname topCellsPerSample
#' @name topCellsPerSample
#' @importFrom AcidGenerics topCellsPerSample
#' @usage topCellsPerSample(object, ...)
#' @export
NULL



#' @rdname tpm
#' @name tpm
#' @importFrom SingleCellExperiment tpm
#' @export
NULL



#' @rdname uniteInterestingGroups
#' @name uniteInterestingGroups
#' @importFrom AcidGenerics uniteInterestingGroups
#' @usage uniteInterestingGroups(object, ...)
#' @export
NULL



#' @rdname upperCamelCase
#' @name upperCamelCase
#' @importFrom syntactic upperCamelCase
#' @usage upperCamelCase(object, ...)
#' @export
NULL



#' @rdname zerosVsDepth
#' @name zerosVsDepth
#' @importFrom AcidGenerics zerosVsDepth
#' @usage zerosVsDepth(object, ...)
#' @export
NULL
