## S3 generics =================================================================

#' @export
#' @importFrom pipette as_tibble
pipette::as_tibble

#' @export
#' @importFrom pipette as.data.table
pipette::as.data.table



## S4 generics =================================================================

#' @export
#' @exportMethod Ensembl2Entrez
#' @importFrom AcidGenomes Ensembl2Entrez
#' @importMethodsFrom AcidGenomes Ensembl2Entrez
AcidGenomes::Ensembl2Entrez

#' @export
#' @exportMethod Entrez2Ensembl
#' @importFrom AcidGenomes Entrez2Ensembl
#' @importMethodsFrom AcidGenomes Entrez2Ensembl
AcidGenomes::Entrez2Ensembl

#' @export
#' @importFrom AcidGenomes Gene2Symbol
AcidGenomes::Gene2Symbol

#' @export
#' @exportMethod Tx2Gene
#' @importFrom AcidGenomes Tx2Gene
#' @importMethodsFrom AcidGenomes Tx2Gene
AcidGenomes::Tx2Gene

#' @export
#' @importFrom AcidGenerics %in%
AcidGenerics::`%in%`

#' @export
#' @exportMethod aggregate
#' @importFrom AcidGenerics aggregate
#' @importMethodsFrom AcidExperiment aggregate
AcidGenerics::aggregate

#' @export
#' @exportMethod aggregateCellsToSamples
#' @importFrom AcidGenerics aggregateCellsToSamples
#' @importMethodsFrom AcidSingleCell aggregateCellsToSamples
AcidGenerics::aggregateCellsToSamples

#' @export
#' @exportMethod aggregateCols
#' @importFrom AcidGenerics aggregateCols
#' @importMethodsFrom AcidExperiment aggregateCols
#' @importMethodsFrom AcidSingleCell aggregateCols
AcidGenerics::aggregateCols

#' @export
#' @exportMethod aggregateRows
#' @importFrom AcidGenerics aggregateRows
#' @importMethodsFrom AcidExperiment aggregateRows
AcidGenerics::aggregateRows

#' @export
#' @exportMethod antiJoin
#' @importFrom AcidGenerics antiJoin
#' @importMethodsFrom AcidPlyr antiJoin
AcidGenerics::antiJoin

#' @export
#' @importFrom AcidGenerics anyDuplicated
AcidGenerics::anyDuplicated

#' @export
#' @importFrom AcidGenerics append
AcidGenerics::append

#' @export
#' @exportMethod as.DataFrame
#' @importFrom AcidGenerics as.DataFrame
#' @importMethodsFrom pipette as.DataFrame
AcidGenerics::as.DataFrame

#' @export
#' @exportMethod as.SummarizedExperiment
#' @importFrom AcidGenerics as.SummarizedExperiment
#' @importMethodsFrom AcidExperiment as.SummarizedExperiment
AcidGenerics::as.SummarizedExperiment

#' @export
#' @exportMethod as.data.frame
#' @importFrom AcidGenerics as.data.frame
#' @importMethodsFrom pipette as.data.frame
AcidGenerics::as.data.frame

#' @export
#' @importFrom AcidGenerics as.factor
AcidGenerics::as.factor

#' @export
#' @importFrom AcidGenerics as.list
AcidGenerics::as.list

#' @export
#' @importFrom AcidGenerics as.matrix
AcidGenerics::as.matrix

#' @export
#' @importFrom AcidGenerics as.table
AcidGenerics::as.table

#' @export
#' @importFrom AcidGenerics as.vector
AcidGenerics::as.vector

#' @export
#' @importFrom AcidExperiment assay
AcidExperiment::assay

#' @export
#' @importFrom AcidExperiment assay<-
AcidExperiment::`assay<-`

#' @export
#' @importFrom AcidExperiment assayNames
AcidExperiment::assayNames

#' @export
#' @importFrom AcidExperiment assayNames<-
AcidExperiment::`assayNames<-`

#' @export
#' @importFrom AcidExperiment assays
AcidExperiment::assays

#' @export
#' @importFrom AcidExperiment assays<-
AcidExperiment::`assays<-`

#' @export
#' @exportMethod atomize
#' @importFrom AcidGenerics atomize
#' @importMethodsFrom pipette atomize
AcidGenerics::atomize

#' @export
#' @exportMethod autopadZeros
#' @importFrom AcidGenerics autopadZeros
#' @importMethodsFrom AcidExperiment autopadZeros
AcidGenerics::autopadZeros

#' @export
#' @importFrom AcidGenerics basename
AcidGenerics::basename

#' @export
#' @exportMethod calculateMetrics
#' @importFrom AcidGenerics calculateMetrics
#' @importMethodsFrom AcidExperiment calculateMetrics
AcidGenerics::calculateMetrics

#' @export
#' @exportMethod camelCase
#' @importFrom AcidGenerics camelCase
#' @importMethodsFrom AcidExperiment camelCase
#' @importMethodsFrom syntactic camelCase
AcidGenerics::camelCase

#' @export
#' @exportMethod capitalize
#' @importFrom AcidGenerics capitalize
#' @importMethodsFrom syntactic capitalize
AcidGenerics::capitalize

#' @export
#' @importFrom AcidGenerics cbind
AcidGenerics::cbind

#' @export
#' @exportMethod cell2sample
#' @importFrom AcidGenerics cell2sample
#' @importMethodsFrom AcidSingleCell cell2sample
AcidGenerics::cell2sample

#' @export
#' @exportMethod coerce
#' @importFrom AcidGenerics coerce
#' @importMethodsFrom pipette coerce
AcidGenerics::coerce

#' @export
#' @exportMethod coerceToList
#' @importFrom AcidGenerics coerceToList
#' @importMethodsFrom AcidBase coerceToList
AcidGenerics::coerceToList

#' @export
#' @importFrom AcidExperiment colData
AcidExperiment::colData

#' @export
#' @importFrom AcidExperiment colData<-
AcidExperiment::`colData<-`

#' @export
#' @importFrom AcidGenerics colSums
AcidGenerics::colSums

#' @export
#' @exportMethod collapseToString
#' @importFrom AcidGenerics collapseToString
#' @importMethodsFrom AcidPlyr collapseToString
AcidGenerics::collapseToString

#' @export
#' @importFrom AcidGenerics colnames
AcidGenerics::colnames

#' @export
#' @importFrom AcidGenerics colnames<-
AcidGenerics::`colnames<-`

#' @export
#' @exportMethod combine
#' @importFrom AcidGenerics combine
#' @importMethodsFrom AcidExperiment combine
#' @importMethodsFrom AcidSingleCell combine
AcidGenerics::combine

#' @export
#' @importFrom AcidGenerics complete.cases
AcidGenerics::complete.cases

#' @export
#' @exportMethod convertGenesToSymbols
#' @importFrom AcidGenerics convertGenesToSymbols
#' @importMethodsFrom AcidExperiment convertGenesToSymbols
AcidGenerics::convertGenesToSymbols

#' @export
#' @exportMethod convertSampleIDsToNames
#' @importFrom AcidGenerics convertSampleIDsToNames
#' @importMethodsFrom AcidExperiment convertSampleIDsToNames
#' @importMethodsFrom AcidSingleCell convertSampleIDsToNames
AcidGenerics::convertSampleIDsToNames

#' @export
#' @exportMethod convertSymbolsToGenes
#' @importFrom AcidGenerics convertSymbolsToGenes
#' @importMethodsFrom AcidExperiment convertSymbolsToGenes
AcidGenerics::convertSymbolsToGenes

#' @export
#' @exportMethod convertTranscriptsToGenes
#' @importFrom AcidGenerics convertTranscriptsToGenes
#' @importMethodsFrom AcidExperiment convertTranscriptsToGenes
AcidGenerics::convertTranscriptsToGenes

#' @export
#' @importFrom AcidGenerics cor
AcidGenerics::cor

#' @export
#' @exportMethod correlation
#' @importFrom AcidGenerics correlation
#' @importMethodsFrom AcidExperiment correlation
AcidGenerics::correlation

#' @export
#' @exportMethod counts
#' @importFrom AcidGenerics counts
#' @importMethodsFrom AcidExperiment counts
AcidGenerics::counts

#' @export
#' @exportMethod counts<-
#' @importFrom AcidGenerics counts<-
#' @importMethodsFrom AcidExperiment counts<-
AcidGenerics::`counts<-`

#' @export
#' @exportMethod decode
#' @importFrom AcidGenerics decode
#' @importMethodsFrom AcidExperiment decode
#' @importMethodsFrom pipette decode
AcidGenerics::decode

#' @export
#' @importFrom AcidGenerics dims
AcidGenerics::dims

#' @export
#' @importFrom AcidGenerics dirname
AcidGenerics::dirname

#' @export
#' @importFrom AcidGenerics do.call
AcidGenerics::do.call

#' @export
#' @exportMethod dottedCase
#' @importFrom AcidGenerics dottedCase
#' @importMethodsFrom AcidExperiment dottedCase
#' @importMethodsFrom syntactic dottedCase
AcidGenerics::dottedCase

#' @export
#' @exportMethod droplevels
#' @importFrom AcidGenerics droplevels
#' @importMethodsFrom AcidExperiment droplevels
#' @importMethodsFrom pipette droplevels
AcidGenerics::droplevels

#' @export
#' @importFrom AcidGenerics duplicated
AcidGenerics::duplicated

#' @export
#' @exportMethod encode
#' @importFrom AcidGenerics encode
#' @importMethodsFrom AcidExperiment encode
#' @importMethodsFrom pipette encode
AcidGenerics::encode

#' @export
#' @importFrom AcidGenerics end
AcidGenerics::end

#' @export
#' @importFrom AcidGenerics end<-
AcidGenerics::`end<-`

#' @export
#' @exportMethod estimateSizeFactors
#' @importFrom AcidGenerics estimateSizeFactors
#' @importMethodsFrom AcidExperiment estimateSizeFactors
AcidGenerics::estimateSizeFactors

#' @export
#' @importFrom AcidGenerics eval
AcidGenerics::eval

#' @export
#' @importFrom AcidGenerics expand
AcidGenerics::expand

#' @export
#' @importFrom AcidGenerics expand.grid
AcidGenerics::expand.grid

#' @export
#' @exportMethod export
#' @importFrom AcidGenerics export
#' @importMethodsFrom AcidExperiment export
#' @importMethodsFrom AcidGenomes export
#' @importMethodsFrom AcidSingleCell export
#' @importMethodsFrom pipette export
AcidGenerics::export

#' @export
#' @exportMethod factorize
#' @importFrom AcidGenerics factorize
#' @importMethodsFrom pipette factorize
AcidGenerics::factorize

#' @export
#' @exportMethod filterCells
#' @importFrom AcidGenerics filterCells
#' @importMethodsFrom AcidSingleCell filterCells
AcidGenerics::filterCells

#' @export
#' @exportMethod foldChangeToLogRatio
#' @importFrom AcidGenerics foldChangeToLogRatio
#' @importMethodsFrom AcidBase foldChangeToLogRatio
AcidGenerics::foldChangeToLogRatio

#' @export
#' @exportMethod fullJoin
#' @importFrom AcidGenerics fullJoin
#' @importMethodsFrom AcidPlyr fullJoin
AcidGenerics::fullJoin

#' @export
#' @exportMethod geneNames
#' @importFrom AcidGenerics geneNames
#' @importMethodsFrom AcidExperiment geneNames
AcidGenerics::geneNames

#' @export
#' @exportMethod geometricMean
#' @importFrom AcidGenerics geometricMean
#' @importMethodsFrom AcidBase geometricMean
#' @importMethodsFrom AcidSingleCell geometricMean
AcidGenerics::geometricMean

#' @export
#' @importFrom AcidGenerics get
AcidGenerics::get

#' @export
#' @importFrom AcidGenerics getListElement
AcidGenerics::getListElement

#' @export
#' @importFrom AcidGenerics grep
AcidGenerics::grep

#' @export
#' @importFrom AcidGenerics grepl
AcidGenerics::grepl

#' @export
#' @importFrom AcidGenerics gsub
AcidGenerics::gsub

#' @export
#' @importFrom AcidGenerics head
AcidGenerics::head

#' @export
#' @exportMethod headtail
#' @importFrom AcidGenerics headtail
#' @importMethodsFrom AcidBase headtail
#' @importMethodsFrom AcidExperiment headtail
AcidGenerics::headtail

#' @export
#' @exportMethod humanize
#' @importFrom AcidGenerics humanize
#' @importMethodsFrom AcidExperiment humanize
AcidGenerics::humanize

#' @export
#' @exportMethod innerJoin
#' @importFrom AcidGenerics innerJoin
#' @importMethodsFrom AcidPlyr innerJoin
AcidGenerics::innerJoin

#' @export
#' @exportMethod integerCounts
#' @importFrom AcidGenerics integerCounts
#' @importMethodsFrom AcidExperiment integerCounts
AcidGenerics::integerCounts

#' @export
#' @exportMethod interestingGroups
#' @importFrom AcidGenerics interestingGroups
#' @importMethodsFrom AcidExperiment interestingGroups
AcidGenerics::interestingGroups

#' @export
#' @exportMethod interestingGroups<-
#' @importFrom AcidGenerics interestingGroups<-
#' @importMethodsFrom AcidExperiment interestingGroups<-
AcidGenerics::`interestingGroups<-`

#' @export
#' @importFrom AcidGenerics intersect
AcidGenerics::intersect

#' @export
#' @exportMethod intersectAll
#' @importFrom AcidGenerics intersectAll
#' @importMethodsFrom AcidBase intersectAll
AcidGenerics::intersectAll

#' @export
#' @exportMethod intersectionMatrix
#' @importFrom AcidGenerics intersectionMatrix
#' @importMethodsFrom AcidBase intersectionMatrix
AcidGenerics::intersectionMatrix

#' @export
#' @importFrom AcidGenerics is.unsorted
AcidGenerics::is.unsorted

#' @export
#' @exportMethod kebabCase
#' @importFrom AcidGenerics kebabCase
#' @importMethodsFrom syntactic kebabCase
AcidGenerics::kebabCase

#' @export
#' @importFrom AcidGenerics lapply
AcidGenerics::lapply

#' @export
#' @exportMethod leftJoin
#' @importFrom AcidGenerics leftJoin
#' @importMethodsFrom AcidPlyr leftJoin
AcidGenerics::leftJoin

#' @export
#' @exportMethod logRatioToFoldChange
#' @importFrom AcidGenerics logRatioToFoldChange
#' @importMethodsFrom AcidBase logRatioToFoldChange
AcidGenerics::logRatioToFoldChange

#' @export
#' @exportMethod makeDimnames
#' @importFrom AcidGenerics makeDimnames
#' @importMethodsFrom syntactic makeDimnames
AcidGenerics::makeDimnames

#' @export
#' @exportMethod makeLabel
#' @importFrom AcidGenerics makeLabel
#' @importMethodsFrom syntactic makeLabel
AcidGenerics::makeLabel

#' @export
#' @exportMethod makeNames
#' @importFrom AcidGenerics makeNames
#' @importMethodsFrom syntactic makeNames
AcidGenerics::makeNames

#' @export
#' @exportMethod makeSampleData
#' @importFrom AcidGenerics makeSampleData
#' @importMethodsFrom AcidExperiment makeSampleData
AcidGenerics::makeSampleData

#' @export
#' @exportMethod makeTitle
#' @importFrom AcidGenerics makeTitle
#' @importMethodsFrom syntactic makeTitle
AcidGenerics::makeTitle

#' @export
#' @exportMethod makeWords
#' @importFrom AcidGenerics makeWords
#' @importMethodsFrom syntactic makeWords
AcidGenerics::makeWords

#' @export
#' @exportMethod mapGenesToIDs
#' @importFrom AcidGenerics mapGenesToIDs
#' @importMethodsFrom AcidExperiment mapGenesToIDs
AcidGenerics::mapGenesToIDs

#' @export
#' @exportMethod mapGenesToRownames
#' @importFrom AcidGenerics mapGenesToRownames
#' @importMethodsFrom AcidExperiment mapGenesToRownames
AcidGenerics::mapGenesToRownames

#' @export
#' @exportMethod mapGenesToSymbols
#' @importFrom AcidGenerics mapGenesToSymbols
#' @importMethodsFrom AcidExperiment mapGenesToSymbols
AcidGenerics::mapGenesToSymbols

#' @export
#' @exportMethod mapToDataFrame
#' @importFrom AcidGenerics mapToDataFrame
#' @importMethodsFrom AcidPlyr mapToDataFrame
AcidGenerics::mapToDataFrame

#' @export
#' @importFrom AcidGenerics mapply
AcidGenerics::mapply

#' @export
#' @importFrom AcidGenerics match
AcidGenerics::match

#' @export
#' @exportMethod matchSampleColumn
#' @importFrom AcidGenerics matchSampleColumn
#' @importMethodsFrom AcidExperiment matchSampleColumn
AcidGenerics::matchSampleColumn

#' @export
#' @importFrom AcidGenerics mcols
AcidGenerics::mcols

#' @export
#' @importFrom AcidGenerics mcols<-
AcidGenerics::`mcols<-`

#' @export
#' @importFrom AcidGenerics mean
AcidGenerics::mean

#' @export
#' @exportMethod melt
#' @importFrom AcidGenerics melt
#' @importMethodsFrom AcidExperiment melt
#' @importMethodsFrom AcidPlyr melt
#' @importMethodsFrom AcidSingleCell melt
AcidGenerics::melt

#' @export
#' @importFrom AcidGenerics merge
AcidGenerics::merge

#' @export
#' @importFrom AcidGenerics metadata
AcidGenerics::metadata

#' @export
#' @importFrom AcidGenerics metadata<-
AcidGenerics::`metadata<-`

#' @export
#' @exportMethod metadata2
#' @importFrom AcidGenerics metadata2
#' @importMethodsFrom pipette metadata2
AcidGenerics::metadata2

#' @export
#' @exportMethod metadata2<-
#' @importFrom AcidGenerics metadata2<-
#' @importMethodsFrom pipette metadata2<-
AcidGenerics::`metadata2<-`

#' @export
#' @exportMethod metrics
#' @importFrom AcidGenerics metrics
#' @importMethodsFrom AcidExperiment metrics
#' @importMethodsFrom AcidSingleCell metrics
AcidGenerics::metrics

#' @export
#' @exportMethod metricsPerSample
#' @importFrom AcidGenerics metricsPerSample
#' @importMethodsFrom AcidSingleCell metricsPerSample
AcidGenerics::metricsPerSample

#' @export
#' @importFrom AcidGenerics mget
AcidGenerics::mget

#' @export
#' @exportMethod mutateAll
#' @importFrom AcidGenerics mutateAll
#' @importMethodsFrom AcidPlyr mutateAll
AcidGenerics::mutateAll

#' @export
#' @exportMethod mutateAt
#' @importFrom AcidGenerics mutateAt
#' @importMethodsFrom AcidPlyr mutateAt
AcidGenerics::mutateAt

#' @export
#' @exportMethod mutateIf
#' @importFrom AcidGenerics mutateIf
#' @importMethodsFrom AcidPlyr mutateIf
AcidGenerics::mutateIf

#' @export
#' @importFrom AcidGenerics na.omit
AcidGenerics::na.omit

#' @export
#' @importFrom AcidGenerics ncol
AcidGenerics::ncol

#' @export
#' @exportMethod nonzeroRowsAndCols
#' @importFrom AcidGenerics nonzeroRowsAndCols
#' @importMethodsFrom AcidExperiment nonzeroRowsAndCols
AcidGenerics::nonzeroRowsAndCols

#' @export
#' @importFrom AcidGenerics nrow
AcidGenerics::nrow

#' @export
#' @importFrom AcidGenerics order
AcidGenerics::order

#' @export
#' @exportMethod organism
#' @importFrom AcidGenerics organism
#' @importMethodsFrom AcidExperiment organism
#' @importMethodsFrom AcidGenomes organism
AcidGenerics::organism

#' @export
#' @exportMethod organism<-
#' @importFrom AcidGenerics organism<-
#' @importMethodsFrom AcidGenomes organism<-
AcidGenerics::`organism<-`

#' @export
#' @importFrom AcidGenerics paste
AcidGenerics::paste

#' @export
#' @importFrom AcidGenerics pmax
AcidGenerics::pmax

#' @export
#' @importFrom AcidGenerics pmax.int
AcidGenerics::pmax.int

#' @export
#' @importFrom AcidGenerics pmin
AcidGenerics::pmin

#' @export
#' @importFrom AcidGenerics pmin.int
AcidGenerics::pmin.int

#' @export
#' @importFrom AcidGenerics pos
AcidGenerics::pos

#' @export
#' @importFrom AcidGenerics ranges
AcidGenerics::ranges

#' @export
#' @importFrom AcidGenerics rank
AcidGenerics::rank

#' @export
#' @exportMethod rankedMatrix
#' @importFrom AcidGenerics rankedMatrix
#' @importMethodsFrom AcidBase rankedMatrix
AcidGenerics::rankedMatrix

#' @export
#' @importFrom AcidGenerics rbind
AcidGenerics::rbind

#' @export
#' @exportMethod rbindToDataFrame
#' @importFrom AcidGenerics rbindToDataFrame
#' @importMethodsFrom AcidPlyr rbindToDataFrame
AcidGenerics::rbindToDataFrame

#' @export
#' @importFrom AcidSingleCell reducedDim
AcidSingleCell::reducedDim

#' @export
#' @importFrom AcidSingleCell reducedDim<-
AcidSingleCell::`reducedDim<-`

#' @export
#' @importFrom AcidSingleCell reducedDimNames
AcidSingleCell::reducedDimNames

#' @export
#' @importFrom AcidSingleCell reducedDimNames<-
AcidSingleCell::`reducedDimNames<-`

#' @export
#' @importFrom AcidSingleCell reducedDims
AcidSingleCell::reducedDims

#' @export
#' @importFrom AcidSingleCell reducedDims<-
AcidSingleCell::`reducedDims<-`

#' @export
#' @importFrom AcidGenerics relist
AcidGenerics::relist

#' @export
#' @exportMethod removeNA
#' @importFrom AcidGenerics removeNA
#' @importMethodsFrom pipette removeNA
AcidGenerics::removeNA

#' @export
#' @importFrom AcidGenerics rep.int
AcidGenerics::rep.int

#' @export
#' @exportMethod rightJoin
#' @importFrom AcidGenerics rightJoin
#' @importMethodsFrom AcidPlyr rightJoin
AcidGenerics::rightJoin

#' @export
#' @importFrom AcidExperiment rowData
AcidExperiment::rowData

#' @export
#' @importFrom AcidExperiment rowData<-
AcidExperiment::`rowData<-`

#' @export
#' @importFrom AcidGenerics rowMeans
AcidGenerics::rowMeans

#' @export
#' @importFrom AcidExperiment rowRanges
AcidExperiment::rowRanges

#' @export
#' @importFrom AcidExperiment rowRanges<-
AcidExperiment::`rowRanges<-`

#' @export
#' @importFrom AcidGenerics rowSums
AcidGenerics::rowSums

#' @export
#' @importFrom AcidGenerics rownames
AcidGenerics::rownames

#' @export
#' @importFrom AcidGenerics rownames<-
AcidGenerics::`rownames<-`

#' @export
#' @exportMethod sampleData
#' @importFrom AcidGenerics sampleData
#' @importMethodsFrom AcidExperiment sampleData
#' @importMethodsFrom AcidSingleCell sampleData
AcidGenerics::sampleData

#' @export
#' @exportMethod sampleData<-
#' @importFrom AcidGenerics sampleData<-
#' @importMethodsFrom AcidExperiment sampleData<-
#' @importMethodsFrom AcidSingleCell sampleData<-
AcidGenerics::`sampleData<-`

#' @export
#' @exportMethod sampleNames
#' @importFrom AcidGenerics sampleNames
#' @importMethodsFrom AcidExperiment sampleNames
AcidGenerics::sampleNames

#' @export
#' @exportMethod sampleNames<-
#' @importFrom AcidGenerics sampleNames<-
#' @importMethodsFrom AcidExperiment sampleNames<-
AcidGenerics::`sampleNames<-`

#' @export
#' @exportMethod sanitizeNA
#' @importFrom AcidGenerics sanitizeNA
#' @importMethodsFrom pipette sanitizeNA
AcidGenerics::sanitizeNA

#' @export
#' @exportMethod sanitizePercent
#' @importFrom AcidGenerics sanitizePercent
#' @importMethodsFrom pipette sanitizePercent
AcidGenerics::sanitizePercent

#' @export
#' @importFrom AcidGenerics sd
AcidGenerics::sd

#' @export
#' @exportMethod selectIf
#' @importFrom AcidGenerics selectIf
#' @importMethodsFrom AcidPlyr selectIf
AcidGenerics::selectIf

#' @export
#' @exportMethod selectSamples
#' @importFrom AcidGenerics selectSamples
#' @importMethodsFrom AcidExperiment selectSamples
#' @importMethodsFrom AcidSingleCell selectSamples
AcidGenerics::selectSamples

#' @export
#' @exportMethod sem
#' @importFrom AcidGenerics sem
#' @importMethodsFrom AcidBase sem
AcidGenerics::sem

#' @export
#' @exportMethod semiJoin
#' @importFrom AcidGenerics semiJoin
#' @importMethodsFrom AcidPlyr semiJoin
AcidGenerics::semiJoin

#' @export
#' @importFrom AcidGenerics setdiff
AcidGenerics::setdiff

#' @export
#' @importFrom pipette seqnames
pipette::seqnames

#' @export
#' @exportMethod showHeader
#' @importFrom AcidGenerics showHeader
#' @importMethodsFrom AcidBase showHeader
AcidGenerics::showHeader

#' @export
#' @exportMethod sizeFactors
#' @importFrom AcidGenerics sizeFactors
#' @importMethodsFrom AcidExperiment sizeFactors
AcidGenerics::sizeFactors

#' @export
#' @exportMethod sizeFactors<-
#' @importFrom AcidGenerics sizeFactors<-
#' @importMethodsFrom AcidExperiment sizeFactors<-
AcidGenerics::`sizeFactors<-`

#' @export
#' @exportMethod snakeCase
#' @importFrom AcidGenerics snakeCase
#' @importMethodsFrom AcidExperiment snakeCase
#' @importMethodsFrom syntactic snakeCase
AcidGenerics::snakeCase

#' @export
#' @importFrom AcidGenerics sort
AcidGenerics::sort

#' @export
#' @importFrom AcidGenerics split
AcidGenerics::split

#' @export
#' @exportMethod splitByLevel
#' @importFrom AcidGenerics splitByLevel
#' @importMethodsFrom AcidPlyr splitByLevel
AcidGenerics::splitByLevel

#' @export
#' @importFrom AcidGenerics start
AcidGenerics::start

#' @export
#' @importFrom AcidGenerics start<-
AcidGenerics::`start<-`

#' @export
#' @exportMethod stripGeneVersions
#' @importFrom AcidGenerics stripGeneVersions
#' @importMethodsFrom AcidExperiment stripGeneVersions
#' @importMethodsFrom AcidGenomes stripGeneVersions
AcidGenerics::stripGeneVersions

#' @export
#' @exportMethod stripTranscriptVersions
#' @importFrom AcidGenerics stripTranscriptVersions
#' @importMethodsFrom AcidExperiment stripTranscriptVersions
#' @importMethodsFrom AcidGenomes stripTranscriptVersions
AcidGenerics::stripTranscriptVersions

#' @export
#' @importFrom AcidGenerics sub
AcidGenerics::sub

#' @export
#' @importFrom AcidGenerics subset
AcidGenerics::subset

#' @export
#' @exportMethod subsetPerSample
#' @importFrom AcidGenerics subsetPerSample
#' @importMethodsFrom AcidSingleCell subsetPerSample
AcidGenerics::subsetPerSample

#' @export
#' @importFrom AcidGenerics summary
AcidGenerics::summary

#' @export
#' @importFrom AcidGenerics t
AcidGenerics::t

#' @export
#' @importFrom AcidGenerics table
AcidGenerics::table

#' @export
#' @importFrom AcidGenerics tail
AcidGenerics::tail

#' @export
#' @importFrom AcidGenerics tapply
AcidGenerics::tapply

#' @export
#' @exportMethod topCellsPerSample
#' @importFrom AcidGenerics subsetPerSample
#' @importMethodsFrom AcidSingleCell topCellsPerSample
AcidGenerics::topCellsPerSample

#' @export
#' @importFrom AcidGenerics trim
AcidGenerics::trim

#' @export
#' @exportMethod tpm
#' @importFrom AcidGenerics tpm
#' @importMethodsFrom AcidExperiment tpm
AcidGenerics::tpm

#' @export
#' @exportMethod transmuteAt
#' @importFrom AcidGenerics transmuteAt
#' @importMethodsFrom AcidPlyr transmuteAt
AcidGenerics::transmuteAt

#' @export
#' @exportMethod transmuteIf
#' @importFrom AcidGenerics transmuteIf
#' @importMethodsFrom AcidPlyr transmuteIf
AcidGenerics::transmuteIf

#' @export
#' @importFrom AcidGenerics union
AcidGenerics::union

#' @export
#' @importFrom AcidGenerics unique
AcidGenerics::unique

#' @export
#' @exportMethod uniteInterestingGroups
#' @importFrom AcidGenerics uniteInterestingGroups
#' @importMethodsFrom AcidExperiment uniteInterestingGroups
AcidGenerics::uniteInterestingGroups

#' @export
#' @importFrom AcidGenerics unlist
AcidGenerics::unlist

## NOTE Remove this in a future update.
#' @export
#' @exportMethod unlistToDataFrame
#' @importFrom AcidGenerics unlistToDataFrame
#' @importMethodsFrom AcidPlyr unlistToDataFrame
AcidGenerics::unlistToDataFrame

#' @export
#' @importFrom AcidGenerics unsplit
AcidGenerics::unsplit

#' @export
#' @exportMethod upperCamelCase
#' @importFrom AcidGenerics upperCamelCase
#' @importMethodsFrom AcidExperiment upperCamelCase
#' @importMethodsFrom syntactic upperCamelCase
AcidGenerics::upperCamelCase

#' @export
#' @importFrom AcidGenerics var
AcidGenerics::var

#' @export
#' @importFrom AcidGenerics which
AcidGenerics::which

#' @export
#' @importFrom AcidGenerics which.max
AcidGenerics::which.max

#' @export
#' @importFrom AcidGenerics which.min
AcidGenerics::which.min

#' @export
#' @importFrom AcidGenerics width
AcidGenerics::width

#' @export
#' @importFrom AcidGenerics width<-
AcidGenerics::`width<-`

#' @export
#' @exportMethod zerosVsDepth
#' @importFrom AcidGenerics zerosVsDepth
#' @importMethodsFrom AcidSingleCell zerosVsDepth
AcidGenerics::zerosVsDepth
