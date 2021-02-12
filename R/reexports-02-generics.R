## S3 generics =================================================================
#' @importFrom pipette as_tibble
#' @export
pipette::as_tibble

#' @importFrom pipette as.data.table
#' @export
pipette::as.data.table



## S4 generics =================================================================
#' @importFrom AcidGenomes Ensembl2Entrez
#' @export
#' @importMethodsFrom AcidGenomes Ensembl2Entrez
#' @exportMethod Ensembl2Entrez
AcidGenomes::Ensembl2Entrez

#' @importFrom AcidGenomes Entrez2Ensembl
#' @export
#' @importMethodsFrom AcidGenomes Entrez2Ensembl
#' @exportMethod Entrez2Ensembl
AcidGenomes::Entrez2Ensembl

#' @importFrom AcidGenomes Gene2Symbol
#' @export
AcidGenomes::Gene2Symbol

#' @importFrom AcidGenomes Tx2Gene
#' @export
#' @importMethodsFrom AcidGenomes Tx2Gene
#' @exportMethod Tx2Gene
AcidGenomes::Tx2Gene

#' @importFrom AcidGenerics %in%
#' @export
AcidGenerics::`%in%`

#' @importFrom AcidGenerics aggregate
#' @export
#' @importMethodsFrom AcidExperiment aggregate
#' @exportMethod aggregate
AcidGenerics::aggregate

#' @importFrom AcidGenerics aggregateCellsToSamples
#' @export
#' @importMethodsFrom AcidSingleCell aggregateCellsToSamples
#' @exportMethod aggregateCellsToSamples
AcidGenerics::aggregateCellsToSamples

#' @importFrom AcidGenerics aggregateCols
#' @export
#' @importMethodsFrom AcidExperiment aggregateCols
#' @importMethodsFrom AcidSingleCell aggregateCols
#' @exportMethod aggregateCols
AcidGenerics::aggregateCols

#' @importFrom AcidGenerics aggregateRows
#' @export
#' @importMethodsFrom AcidExperiment aggregateRows
#' @exportMethod aggregateRows
AcidGenerics::aggregateRows

#' @importFrom AcidGenerics antiJoin
#' @export
#' @importMethodsFrom AcidPlyr antiJoin
#' @exportMethod antiJoin
AcidGenerics::antiJoin

#' @importFrom AcidGenerics as.DataFrame
#' @export
#' @importMethodsFrom pipette as.DataFrame
#' @exportMethod as.DataFrame
AcidGenerics::as.DataFrame

#' @importFrom AcidGenerics as.SummarizedExperiment
#' @export
#' @importMethodsFrom AcidExperiment as.SummarizedExperiment
#' @exportMethod as.SummarizedExperiment
AcidGenerics::as.SummarizedExperiment

#' @importFrom AcidGenerics as.data.frame
#' @export
#' @importMethodsFrom pipette as.data.frame
#' @exportMethod as.data.frame
AcidGenerics::as.data.frame

#' @importFrom AcidGenerics as.list
#' @export
AcidGenerics::as.list

#' @importFrom AcidExperiment assay
#' @export
AcidExperiment::assay

#' @importFrom AcidExperiment assay<-
#' @export
AcidExperiment::`assay<-`

#' @importFrom AcidExperiment assayNames
#' @export
AcidExperiment::assayNames

#' @importFrom AcidExperiment assayNames<-
#' @export
AcidExperiment::`assayNames<-`

#' @importFrom AcidExperiment assays
#' @export
AcidExperiment::assays

#' @importFrom AcidExperiment assays<-
#' @export
AcidExperiment::`assays<-`

#' @importFrom AcidGenerics atomize
#' @export
#' @importMethodsFrom pipette atomize
#' @exportMethod atomize
AcidGenerics::atomize

#' @importFrom AcidGenerics autopadZeros
#' @export
#' @importMethodsFrom AcidExperiment autopadZeros
#' @exportMethod autopadZeros
AcidGenerics::autopadZeros

#' @importFrom AcidGenerics calculateMetrics
#' @export
#' @importMethodsFrom AcidExperiment calculateMetrics
#' @exportMethod calculateMetrics
AcidGenerics::calculateMetrics

#' @importFrom AcidGenerics camelCase
#' @export
#' @importMethodsFrom AcidExperiment camelCase
#' @importMethodsFrom syntactic camelCase
#' @exportMethod camelCase
AcidGenerics::camelCase

#' @importFrom AcidGenerics capitalize
#' @export
#' @importMethodsFrom syntactic capitalize
#' @exportMethod capitalize
AcidGenerics::capitalize

#' @importFrom AcidGenerics cbind
#' @export
AcidGenerics::cbind

#' @importFrom AcidGenerics cell2sample
#' @export
#' @importMethodsFrom AcidSingleCell cell2sample
#' @exportMethod cell2sample
AcidGenerics::cell2sample

#' @importFrom AcidGenerics coerce
#' @export
#' @importMethodsFrom pipette coerce
#' @exportMethod coerce
AcidGenerics::coerce

#' @importFrom AcidGenerics coerceToList
#' @export
#' @importMethodsFrom AcidBase coerceToList
#' @exportMethod coerceToList
AcidGenerics::coerceToList

#' @importFrom AcidExperiment colData
#' @export
AcidExperiment::colData

#' @importFrom AcidExperiment colData<-
#' @export
AcidExperiment::`colData<-`

#' @importFrom AcidGenerics colSums
#' @export
AcidGenerics::colSums

#' @importFrom AcidGenerics collapseToString
#' @export
#' @importMethodsFrom AcidPlyr collapseToString
#' @exportMethod collapseToString
AcidGenerics::collapseToString

#' @importFrom AcidGenerics combine
#' @export
#' @importMethodsFrom AcidExperiment combine
#' @importMethodsFrom AcidSingleCell combine
#' @exportMethod combine
AcidGenerics::combine

#' @importFrom AcidGenerics complete.cases
#' @export
AcidGenerics::complete.cases

#' @importFrom AcidGenerics convertGenesToSymbols
#' @export
#' @importMethodsFrom AcidExperiment convertGenesToSymbols
#' @exportMethod convertGenesToSymbols
AcidGenerics::convertGenesToSymbols

#' @importFrom AcidGenerics convertSampleIDsToNames
#' @export
#' @importMethodsFrom AcidExperiment convertSampleIDsToNames
#' @importMethodsFrom AcidSingleCell convertSampleIDsToNames
#' @exportMethod convertSampleIDsToNames
AcidGenerics::convertSampleIDsToNames

#' @importFrom AcidGenerics convertSymbolsToGenes
#' @export
#' @importMethodsFrom AcidExperiment convertSymbolsToGenes
#' @exportMethod convertSymbolsToGenes
AcidGenerics::convertSymbolsToGenes

#' @importFrom AcidGenerics convertTranscriptsToGenes
#' @export
#' @importMethodsFrom AcidExperiment convertTranscriptsToGenes
#' @exportMethod convertTranscriptsToGenes
AcidGenerics::convertTranscriptsToGenes

#' @importFrom AcidGenerics cor
#' @export
AcidGenerics::cor

#' @importFrom AcidGenerics correlation
#' @export
#' @importMethodsFrom AcidExperiment correlation
#' @exportMethod correlation
AcidGenerics::correlation

#' @importFrom AcidGenerics counts
#' @export
#' @importMethodsFrom AcidExperiment counts
#' @exportMethod counts
AcidGenerics::counts

#' @importFrom AcidGenerics counts<-
#' @export
#' @importMethodsFrom AcidExperiment counts<-
#' @exportMethod counts<-
AcidGenerics::`counts<-`

#' @importFrom AcidGenerics decode
#' @export
#' @importMethodsFrom AcidExperiment decode
#' @importMethodsFrom pipette decode
#' @exportMethod decode
AcidGenerics::decode

#' @importFrom AcidGenerics do.call
#' @export
AcidGenerics::do.call

#' @importFrom AcidGenerics dottedCase
#' @export
#' @importMethodsFrom AcidExperiment dottedCase
#' @importMethodsFrom syntactic dottedCase
#' @exportMethod dottedCase
AcidGenerics::dottedCase

#' @importFrom AcidGenerics droplevels
#' @export
#' @importMethodsFrom AcidExperiment droplevels
#' @importMethodsFrom pipette droplevels
#' @exportMethod droplevels
AcidGenerics::droplevels

#' @importFrom AcidGenerics encode
#' @export
#' @importMethodsFrom AcidExperiment encode
#' @importMethodsFrom pipette encode
#' @exportMethod encode
AcidGenerics::encode

#' @importFrom AcidGenerics end
#' @export
AcidGenerics::end

#' @importFrom AcidGenerics estimateSizeFactors
#' @export
#' @importMethodsFrom AcidExperiment estimateSizeFactors
#' @exportMethod estimateSizeFactors
AcidGenerics::estimateSizeFactors

#' @importFrom AcidGenerics expand
#' @export
AcidGenerics::expand

#' @importFrom AcidGenerics expand.grid
#' @export
AcidGenerics::expand.grid

#' @importFrom AcidGenerics export
#' @export
#' @importMethodsFrom AcidExperiment export
#' @importMethodsFrom AcidGenomes export
#' @importMethodsFrom AcidSingleCell export
#' @importMethodsFrom pipette export
#' @exportMethod export
AcidGenerics::export

#' @importFrom AcidGenerics factorize
#' @export
#' @importMethodsFrom pipette factorize
#' @exportMethod factorize
AcidGenerics::factorize

#' @importFrom AcidGenerics filterCells
#' @export
#' @importMethodsFrom AcidSingleCell filterCells
#' @exportMethod filterCells
AcidGenerics::filterCells

#' @importFrom AcidGenerics foldChangeToLogRatio
#' @export
#' @importMethodsFrom AcidBase foldChangeToLogRatio
#' @exportMethod foldChangeToLogRatio
AcidGenerics::foldChangeToLogRatio

#' @importFrom AcidGenerics fullJoin
#' @export
#' @importMethodsFrom AcidPlyr fullJoin
#' @exportMethod fullJoin
AcidGenerics::fullJoin

#' @importFrom AcidGenerics geneNames
#' @export
#' @importMethodsFrom AcidExperiment geneNames
#' @exportMethod geneNames
AcidGenerics::geneNames

#' @importFrom AcidGenerics geometricMean
#' @export
#' @importMethodsFrom AcidBase geometricMean
#' @importMethodsFrom AcidSingleCell geometricMean
#' @exportMethod geometricMean
AcidGenerics::geometricMean

#' @importFrom AcidGenerics grep
#' @export
AcidGenerics::grep

#' @importFrom AcidGenerics grepl
#' @export
AcidGenerics::grepl

#' @importFrom AcidGenerics head
#' @export
AcidGenerics::head

#' @importFrom AcidGenerics headtail
#' @export
#' @importMethodsFrom AcidBase headtail
#' @importMethodsFrom AcidExperiment headtail
#' @exportMethod headtail
AcidGenerics::headtail

#' @importFrom AcidGenerics humanize
#' @export
#' @importMethodsFrom AcidExperiment humanize
#' @exportMethod humanize
AcidGenerics::humanize

#' @importFrom AcidGenerics innerJoin
#' @export
#' @importMethodsFrom AcidPlyr innerJoin
#' @exportMethod innerJoin
AcidGenerics::innerJoin

#' @importFrom AcidGenerics integerCounts
#' @export
#' @importMethodsFrom AcidExperiment integerCounts
#' @exportMethod integerCounts
AcidGenerics::integerCounts

#' @importFrom AcidGenerics interestingGroups
#' @export
#' @importMethodsFrom AcidExperiment interestingGroups
#' @exportMethod interestingGroups
AcidGenerics::interestingGroups

#' @importFrom AcidGenerics interestingGroups<-
#' @export
#' @importMethodsFrom AcidExperiment interestingGroups<-
#' @exportMethod interestingGroups<-
AcidGenerics::`interestingGroups<-`
#' @importFrom AcidGenerics intersectAll
#' @export
#' @importMethodsFrom AcidBase intersectAll
#' @exportMethod intersectAll
AcidGenerics::intersectAll

#' @importFrom AcidGenerics intersectionMatrix
#' @export
#' @importMethodsFrom AcidBase intersectionMatrix
#' @exportMethod intersectionMatrix
AcidGenerics::intersectionMatrix

#' @importFrom AcidGenerics is.unsorted
#' @export
AcidGenerics::is.unsorted

#' @importFrom AcidGenerics kebabCase
#' @export
#' @importMethodsFrom syntactic kebabCase
#' @exportMethod kebabCase
AcidGenerics::kebabCase

#' @importFrom AcidGenerics lapply
#' @export
AcidGenerics::lapply

#' @importFrom AcidGenerics leftJoin
#' @export
#' @importMethodsFrom AcidPlyr leftJoin
#' @exportMethod leftJoin
AcidGenerics::leftJoin

#' @importFrom AcidGenerics logRatioToFoldChange
#' @export
#' @importMethodsFrom AcidBase logRatioToFoldChange
#' @exportMethod logRatioToFoldChange
AcidGenerics::logRatioToFoldChange

#' @importFrom AcidGenerics makeDimnames
#' @export
#' @importMethodsFrom syntactic makeDimnames
#' @exportMethod makeDimnames
AcidGenerics::makeDimnames

#' @importFrom AcidGenerics makeLabel
#' @export
#' @importMethodsFrom syntactic makeLabel
#' @exportMethod makeLabel
AcidGenerics::makeLabel

#' @importFrom AcidGenerics makeNames
#' @export
#' @importMethodsFrom syntactic makeNames
#' @exportMethod makeNames
AcidGenerics::makeNames

#' @importFrom AcidGenerics makeSampleData
#' @export
#' @importMethodsFrom AcidExperiment makeSampleData
#' @exportMethod makeSampleData
AcidGenerics::makeSampleData

#' @importFrom AcidGenerics makeTitle
#' @export
#' @importMethodsFrom syntactic makeTitle
#' @exportMethod makeTitle
AcidGenerics::makeTitle

#' @importFrom AcidGenerics makeWords
#' @export
#' @importMethodsFrom syntactic makeWords
#' @exportMethod makeWords
AcidGenerics::makeWords

#' @importFrom AcidGenerics mapGenesToIDs
#' @export
#' @importMethodsFrom AcidExperiment mapGenesToIDs
#' @exportMethod mapGenesToIDs
AcidGenerics::mapGenesToIDs

#' @importFrom AcidGenerics mapGenesToRownames
#' @export
#' @importMethodsFrom AcidExperiment mapGenesToRownames
#' @exportMethod mapGenesToRownames
AcidGenerics::mapGenesToRownames

#' @importFrom AcidGenerics mapGenesToSymbols
#' @export
#' @importMethodsFrom AcidExperiment mapGenesToSymbols
#' @exportMethod mapGenesToSymbols
AcidGenerics::mapGenesToSymbols

#' @importFrom AcidGenerics match
#' @export
AcidGenerics::match

#' @importFrom AcidGenerics matchSampleColumn
#' @export
#' @importMethodsFrom AcidExperiment matchSampleColumn
#' @exportMethod matchSampleColumn
AcidGenerics::matchSampleColumn

#' @importFrom AcidGenerics mcols
#' @export
AcidGenerics::mcols

#' @importFrom AcidGenerics mcols<-
#' @export
AcidGenerics::`mcols<-`

#' @importFrom AcidGenerics melt
#' @export
#' @importMethodsFrom AcidExperiment melt
#' @importMethodsFrom AcidPlyr melt
#' @importMethodsFrom AcidSingleCell melt
#' @exportMethod melt
AcidGenerics::melt

#' @importFrom AcidGenerics merge
#' @export
AcidGenerics::merge

#' @importFrom AcidGenerics metadata
#' @export
AcidGenerics::metadata

#' @importFrom AcidGenerics metadata<-
#' @export
AcidGenerics::`metadata<-`

#' @importFrom AcidGenerics metadata2
#' @export
#' @importMethodsFrom pipette metadata2
#' @exportMethod metadata2
AcidGenerics::metadata2

#' @importFrom AcidGenerics metadata2<-
#' @export
#' @importMethodsFrom pipette metadata2<-
#' @exportMethod metadata2<-
AcidGenerics::`metadata2<-`

#' @importFrom AcidGenerics metrics
#' @export
#' @importMethodsFrom AcidExperiment metrics
#' @importMethodsFrom AcidSingleCell metrics
#' @exportMethod metrics
AcidGenerics::metrics

#' @importFrom AcidGenerics metricsPerSample
#' @export
#' @importMethodsFrom AcidSingleCell metricsPerSample
#' @exportMethod metricsPerSample
AcidGenerics::metricsPerSample

#' @importFrom AcidGenerics mutateAll
#' @export
#' @importMethodsFrom AcidPlyr mutateAll
#' @exportMethod mutateAll
AcidGenerics::mutateAll

#' @importFrom AcidGenerics mutateAt
#' @export
#' @importMethodsFrom AcidPlyr mutateAt
#' @exportMethod mutateAt
AcidGenerics::mutateAt

#' @importFrom AcidGenerics mutateIf
#' @export
#' @importMethodsFrom AcidPlyr mutateIf
#' @exportMethod mutateIf
AcidGenerics::mutateIf

#' @importFrom AcidGenerics na.omit
#' @export
AcidGenerics::na.omit

#' @importFrom AcidGenerics nonzeroRowsAndCols
#' @export
#' @importMethodsFrom AcidExperiment nonzeroRowsAndCols
#' @exportMethod nonzeroRowsAndCols
AcidGenerics::nonzeroRowsAndCols

#' @importFrom AcidGenerics order
#' @export
AcidGenerics::order

#' @importFrom AcidGenerics organism
#' @export
#' @importMethodsFrom AcidExperiment organism
#' @importMethodsFrom AcidGenomes organism
#' @exportMethod organism
AcidGenerics::organism

#' @importFrom AcidGenerics organism<-
#' @export
#' @importMethodsFrom AcidGenomes organism<-
#' @exportMethod organism<-
AcidGenerics::`organism<-`

#' @importFrom AcidGenerics rankedMatrix
#' @export
#' @importMethodsFrom AcidBase rankedMatrix
#' @exportMethod rankedMatrix
AcidGenerics::rankedMatrix

#' @importFrom AcidGenerics rbind
#' @export
AcidGenerics::rbind

#' @importFrom AcidSingleCell reducedDim
#' @export
AcidSingleCell::reducedDim

#' @importFrom AcidSingleCell reducedDim<-
#' @export
AcidSingleCell::`reducedDim<-`

#' @importFrom AcidSingleCell reducedDimNames
#' @export
AcidSingleCell::reducedDimNames

#' @importFrom AcidSingleCell reducedDimNames<-
#' @export
AcidSingleCell::`reducedDimNames<-`

#' @importFrom AcidSingleCell reducedDims
#' @export
AcidSingleCell::reducedDims

#' @importFrom AcidSingleCell reducedDims<-
#' @export
AcidSingleCell::`reducedDims<-`

#' @importFrom AcidGenerics removeNA
#' @export
#' @importMethodsFrom pipette removeNA
#' @exportMethod removeNA
AcidGenerics::removeNA

#' @importFrom AcidGenerics rightJoin
#' @export
#' @importMethodsFrom AcidPlyr rightJoin
#' @exportMethod rightJoin
AcidGenerics::rightJoin

#' @importFrom AcidExperiment rowData
#' @export
AcidExperiment::rowData

#' @importFrom AcidExperiment rowData<-
#' @export
AcidExperiment::`rowData<-`

#' @importFrom AcidGenerics rowMeans
#' @export
AcidGenerics::rowMeans

#' @importFrom AcidExperiment rowRanges
#' @export
AcidExperiment::rowRanges

#' @importFrom AcidExperiment rowRanges<-
#' @export
AcidExperiment::`rowRanges<-`

#' @importFrom AcidGenerics rowSums
#' @export
AcidGenerics::rowSums

#' @importFrom AcidGenerics sampleData
#' @export
#' @importMethodsFrom AcidExperiment sampleData
#' @importMethodsFrom AcidSingleCell sampleData
#' @exportMethod sampleData
AcidGenerics::sampleData

#' @importFrom AcidGenerics sampleData<-
#' @export
#' @importMethodsFrom AcidExperiment sampleData<-
#' @importMethodsFrom AcidSingleCell sampleData<-
#' @exportMethod sampleData<-
AcidGenerics::`sampleData<-`

#' @importFrom AcidGenerics sampleNames
#' @export
#' @importMethodsFrom AcidExperiment sampleNames
#' @exportMethod sampleNames
AcidGenerics::sampleNames

#' @importFrom AcidGenerics sampleNames<-
#' @export
#' @importMethodsFrom AcidExperiment sampleNames<-
#' @exportMethod sampleNames<-
AcidGenerics::`sampleNames<-`

#' @importFrom AcidGenerics sanitizeNA
#' @export
#' @importMethodsFrom pipette sanitizeNA
#' @exportMethod sanitizeNA
AcidGenerics::sanitizeNA

#' @importFrom AcidGenerics sanitizePercent
#' @export
#' @importMethodsFrom pipette sanitizePercent
#' @exportMethod sanitizePercent
AcidGenerics::sanitizePercent

#' @importFrom AcidGenerics selectIf
#' @export
#' @importMethodsFrom AcidPlyr selectIf
#' @exportMethod selectIf
AcidGenerics::selectIf

#' @importFrom AcidGenerics selectSamples
#' @export
#' @importMethodsFrom AcidExperiment selectSamples
#' @importMethodsFrom AcidSingleCell selectSamples
#' @exportMethod selectSamples
AcidGenerics::selectSamples

#' @importFrom AcidGenerics sem
#' @export
#' @importMethodsFrom AcidBase sem
#' @exportMethod sem
AcidGenerics::sem

#' @importFrom AcidGenerics semiJoin
#' @export
#' @importMethodsFrom AcidPlyr semiJoin
#' @exportMethod semiJoin
AcidGenerics::semiJoin

#' @importFrom AcidGenerics showHeader
#' @export
#' @importMethodsFrom AcidBase showHeader
#' @exportMethod showHeader
AcidGenerics::showHeader

#' @importFrom AcidGenerics sizeFactors
#' @export
#' @importMethodsFrom AcidExperiment sizeFactors
#' @exportMethod sizeFactors
AcidGenerics::sizeFactors

#' @importFrom AcidGenerics sizeFactors<-
#' @export
#' @importMethodsFrom AcidExperiment sizeFactors<-
#' @exportMethod sizeFactors<-
AcidGenerics::`sizeFactors<-`

#' @importFrom AcidGenerics snakeCase
#' @export
#' @importMethodsFrom AcidExperiment snakeCase
#' @importMethodsFrom syntactic snakeCase
#' @exportMethod snakeCase
AcidGenerics::snakeCase

#' @importFrom AcidGenerics sort
#' @export
AcidGenerics::sort

#' @importFrom AcidGenerics split
#' @export
AcidGenerics::split

#' @importFrom AcidGenerics splitByLevel
#' @export
#' @importMethodsFrom AcidPlyr splitByLevel
#' @exportMethod splitByLevel
AcidGenerics::splitByLevel

#' @importFrom AcidGenerics start
#' @export
AcidGenerics::start

#' @importFrom AcidGenerics stripGeneVersions
#' @export
#' @importMethodsFrom AcidExperiment stripGeneVersions
#' @importMethodsFrom AcidGenomes stripGeneVersions
#' @exportMethod stripGeneVersions
AcidGenerics::stripGeneVersions

#' @importFrom AcidGenerics stripTranscriptVersions
#' @export
#' @importMethodsFrom AcidExperiment stripTranscriptVersions
#' @importMethodsFrom AcidGenomes stripTranscriptVersions
#' @exportMethod stripTranscriptVersions
AcidGenerics::stripTranscriptVersions

#' @importFrom AcidGenerics subsetPerSample
#' @export
#' @importMethodsFrom AcidSingleCell subsetPerSample
#' @exportMethod subsetPerSample
AcidGenerics::subsetPerSample

#' @importFrom AcidGenerics summary
#' @export
AcidGenerics::summary

#' @importFrom AcidGenerics subsetPerSample
#' @export
#' @importMethodsFrom AcidSingleCell topCellsPerSample
#' @exportMethod topCellsPerSample
AcidGenerics::topCellsPerSample

#' @importFrom AcidGenerics t
#' @export
AcidGenerics::t

#' @importFrom AcidGenerics table
#' @export
AcidGenerics::table

#' @importFrom AcidGenerics tail
#' @export
AcidGenerics::tail

#' @importFrom AcidGenerics tpm
#' @export
#' @importMethodsFrom AcidExperiment tpm
#' @exportMethod tpm
AcidGenerics::tpm

#' @importFrom AcidGenerics transmuteAt
#' @export
#' @importMethodsFrom AcidPlyr transmuteAt
#' @exportMethod transmuteAt
AcidGenerics::transmuteAt

#' @importFrom AcidGenerics transmuteIf
#' @export
#' @importMethodsFrom AcidPlyr transmuteIf
#' @exportMethod transmuteIf
AcidGenerics::transmuteIf

#' @importFrom AcidGenerics unique
#' @export
AcidGenerics::unique

#' @importFrom AcidGenerics uniteInterestingGroups
#' @export
#' @importMethodsFrom AcidExperiment uniteInterestingGroups
#' @exportMethod uniteInterestingGroups
AcidGenerics::uniteInterestingGroups

#' @importFrom AcidGenerics unlist
#' @export
AcidGenerics::unlist

#' @importFrom AcidGenerics unlistToDataFrame
#' @export
#' @importMethodsFrom AcidPlyr unlistToDataFrame
#' @exportMethod unlistToDataFrame
AcidGenerics::unlistToDataFrame

#' @importFrom AcidGenerics unsplit
#' @export
AcidGenerics::unsplit

#' @importFrom AcidGenerics upperCamelCase
#' @export
#' @importMethodsFrom AcidExperiment upperCamelCase
#' @importMethodsFrom syntactic upperCamelCase
#' @exportMethod upperCamelCase
AcidGenerics::upperCamelCase

#' @importFrom AcidGenerics var
#' @export
AcidGenerics::var

#' @importFrom AcidGenerics width
#' @export
AcidGenerics::width

#' @importFrom AcidGenerics zerosVsDepth
#' @export
#' @importMethodsFrom AcidSingleCell zerosVsDepth
#' @exportMethod zerosVsDepth
AcidGenerics::zerosVsDepth
