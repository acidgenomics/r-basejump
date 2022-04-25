## S3 generics =================================================================

#' @export
#' @importFrom pipette as_tibble
#' @name as_tibble
#' @rdname reexports
NULL

#' @export
#' @importFrom pipette as.data.table
#' @name as.data.table
#' @rdname reexports
NULL



## S4 generics =================================================================

#' @export
#' @exportMethod Ensembl2Entrez
#' @importFrom AcidGenerics Ensembl2Entrez
#' @importMethodsFrom AcidGenomes Ensembl2Entrez
#' @name Ensembl2Entrez
#' @rdname reexports
NULL

#' @export
#' @exportMethod Entrez2Ensembl
#' @importFrom AcidGenerics Entrez2Ensembl
#' @importMethodsFrom AcidGenomes Entrez2Ensembl
#' @name Entrez2Ensembl
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics Filter
#' @name Filter
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics Find
#' @name Find
#' @rdname reexports
NULL

#' @export
#' @exportMethod Gene2Symbol
#' @importFrom AcidGenerics Gene2Symbol
#' @importMethodsFrom AcidGenomes Gene2Symbol
#' @name Gene2Symbol
#' @rdname reexports
NULL

#' @export
#' @importFrom AcidGenerics KnownMarkers
#' @name KnownMarkers
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics Map
#' @name Map
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics Position
#' @name Position
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics Reduce
#' @name Reduce
#' @rdname reexports
NULL

#' @export
#' @exportMethod Tx2Gene
#' @importFrom AcidGenerics Tx2Gene
#' @importMethodsFrom AcidGenomes Tx2Gene
#' @name Tx2Gene
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics %in%
#' @name %in%
#' @rdname reexports
NULL

#' @export
#' @exportMethod aggregate
#' @importFrom S4Vectors aggregate
#' @importMethodsFrom AcidExperiment aggregate
#' @importMethodsFrom AcidSingleCell aggregate
#' @name aggregate
#' @rdname reexports
NULL

#' @export
#' @exportMethod aggregateCellsToSamples
#' @importFrom AcidGenerics aggregateCellsToSamples
#' @importMethodsFrom AcidSingleCell aggregateCellsToSamples
#' @name aggregateCellsToSamples
#' @rdname reexports
NULL

#' @export
#' @exportMethod aggregateCols
#' @importFrom AcidGenerics aggregateCols
#' @importMethodsFrom AcidExperiment aggregateCols
#' @name aggregateCols
#' @rdname reexports
NULL

#' @export
#' @exportMethod aggregateRows
#' @importFrom AcidGenerics aggregateRows
#' @importMethodsFrom AcidExperiment aggregateRows
#' @name aggregateRows
#' @rdname reexports
NULL

#' @export
#' @exportMethod antiJoin
#' @importFrom AcidGenerics antiJoin
#' @importMethodsFrom AcidPlyr antiJoin
#' @name antiJoin
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics anyDuplicated
#' @name anyDuplicated
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics append
#' @name append
#' @rdname reexports
NULL

#' @export
#' @exportMethod as.DataFrame
#' @importFrom AcidGenerics as.DataFrame
#' @importMethodsFrom pipette as.DataFrame
#' @name as.DataFrame
#' @rdname reexports
NULL

#' @export
#' @exportMethod as.SummarizedExperiment
#' @importFrom AcidGenerics as.SummarizedExperiment
#' @importMethodsFrom AcidExperiment as.SummarizedExperiment
#' @name as.SummarizedExperiment
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics as.data.frame
#' @name as.data.frame
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors as.factor
#' @name as.factor
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics as.list
#' @name as.list
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors as.matrix
#' @name as.matrix
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors as.table
#' @name as.table
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics as.vector
#' @name as.vector
#' @rdname reexports
NULL

#' @aliases assay<-
#' @export assay assay<-
#' @importFrom SummarizedExperiment assay assay<-
#' @name assay
#' @rdname reexports
NULL

#' @aliases assayNames<-
#' @export assayNames assayNames<-
#' @importFrom SummarizedExperiment assayNames assayNames<-
#' @name assayNames
#' @rdname reexports
NULL


#' @aliases assays<-
#' @export assays assays<-
#' @importFrom SummarizedExperiment assays assays<-
#' @name assays
#' @rdname reexports
NULL

#' @export
#' @exportMethod atomize
#' @importFrom AcidGenerics atomize
#' @importMethodsFrom pipette atomize
#' @name atomize
#' @rdname reexports
NULL

#' @export
#' @exportMethod autopadZeros
#' @importFrom AcidGenerics autopadZeros
#' @importMethodsFrom AcidExperiment autopadZeros
#' @importMethodsFrom syntactic autopadZeros
#' @name autopadZeros
#' @rdname reexports
NULL

#' @export
#' @exportMethod barcodeRanksPerSample
#' @importFrom AcidGenerics barcodeRanksPerSample
#' @importMethodsFrom AcidSingleCell barcodeRanksPerSample
#' @name barcodeRanksPerSample
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics basename
#' @name basename
#' @rdname reexports
NULL

#' @export
#' @exportMethod calculateMetrics
#' @importFrom AcidGenerics calculateMetrics
#' @importMethodsFrom AcidExperiment calculateMetrics
#' @name calculateMetrics
#' @rdname reexports
NULL

#' @export
#' @exportMethod camelCase
#' @importFrom AcidGenerics camelCase
#' @importMethodsFrom AcidExperiment camelCase
#' @importMethodsFrom syntactic camelCase
#' @name camelCase
#' @rdname reexports
NULL

#' @export
#' @exportMethod capitalize
#' @importFrom AcidGenerics capitalize
#' @importMethodsFrom syntactic capitalize
#' @name capitalize
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics cbind
#' @name cbind
#' @rdname reexports
NULL

#' @export
#' @exportMethod cell2sample
#' @importFrom AcidGenerics cell2sample
#' @importMethodsFrom AcidSingleCell cell2sample
#' @name cell2sample
#' @rdname reexports
NULL

#' @export
#' @exportMethod cellCountsPerCluster
#' @importFrom AcidGenerics cellCountsPerCluster
#' @importMethodsFrom AcidSingleCell cellCountsPerCluster
#' @name cellCountsPerCluster
#' @rdname reexports
NULL

#' @export
#' @exportMethod cellTypesPerCluster
#' @importFrom AcidGenerics cellTypesPerCluster
#' @importMethodsFrom AcidSingleCell cellTypesPerCluster
#' @name cellTypesPerCluster
#' @rdname reexports
NULL

#' @export
#' @exportMethod clusters
#' @importFrom AcidGenerics clusters
#' @importMethodsFrom AcidSingleCell clusters
#' @name clusters
#' @rdname reexports
NULL

#' @export
#' @exportMethod coerce
#' @importFrom methods coerce
#' @importMethodsFrom pipette coerce
#' @name coerce
#' @rdname coerce
NULL

#' @export
#' @exportMethod coerceToList
#' @importFrom AcidGenerics coerceToList
#' @importMethodsFrom AcidBase coerceToList
#' @name coerceToList
#' @rdname reexports
NULL

#' @aliases colData<-
#' @export colData colData<-
#' @importFrom SummarizedExperiment colData colData<-
#' @name colData
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics colSums
#' @name colSums
#' @rdname reexports
NULL

#' @export
#' @exportMethod collapseToString
#' @importFrom AcidGenerics collapseToString
#' @importMethodsFrom AcidPlyr collapseToString
#' @name collapseToString
#' @rdname reexports
NULL

#' @aliases colnames<-
#' @export colnames colnames<-
#' @importFrom BiocGenerics colnames colnames<-
#' @name colnames
#' @rdname reexports
NULL

#' @export
#' @exportMethod combine
#' @importFrom BiocGenerics combine
#' @importMethodsFrom AcidExperiment combine
#' @importMethodsFrom AcidSingleCell combine
#' @name combine
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors complete.cases
#' @name complete.cases
#' @rdname reexports
NULL

#' @export
#' @exportMethod convertGenesToSymbols
#' @importFrom AcidGenerics convertGenesToSymbols
#' @importMethodsFrom AcidExperiment convertGenesToSymbols
#' @name convertGenesToSymbols
#' @rdname reexports
NULL

#' @export
#' @exportMethod convertSampleIDsToNames
#' @importFrom AcidGenerics convertSampleIDsToNames
#' @importMethodsFrom AcidExperiment convertSampleIDsToNames
#' @importMethodsFrom AcidSingleCell convertSampleIDsToNames
#' @name convertSampleIDsToNames
#' @rdname reexports
NULL

#' @export
#' @exportMethod convertSymbolsToGenes
#' @importFrom AcidGenerics convertSymbolsToGenes
#' @importMethodsFrom AcidExperiment convertSymbolsToGenes
#' @name convertSymbolsToGenes
#' @rdname reexports
NULL

#' @export
#' @exportMethod convertTranscriptsToGenes
#' @importFrom AcidGenerics convertTranscriptsToGenes
#' @importMethodsFrom AcidExperiment convertTranscriptsToGenes
#' @name convertTranscriptsToGenes
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors cor
#' @name cor
#' @rdname reexports
NULL

#' @export
#' @exportMethod correlation
#' @importFrom AcidGenerics correlation
#' @importMethodsFrom AcidExperiment correlation
#' @name correlation
#' @rdname reexports
NULL

#' @aliases counts<-
#' @export
#' @exportMethod counts counts<-
#' @importFrom BiocGenerics counts counts<-
#' @importMethodsFrom AcidExperiment counts counts<-
#' @name counts
#' @rdname reexports
NULL

#' @export
#' @exportMethod cpm
#' @importFrom AcidGenerics cpm
#' @importMethodsFrom AcidSingleCell cpm
#' @name cpm
#' @rdname reexports
NULL

#' @export
#' @exportMethod decode
#' @importFrom S4Vectors decode
#' @importMethodsFrom AcidExperiment decode
#' @importMethodsFrom pipette decode
#' @name decode
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics design
#' @name design
#' @rdname reexports
NULL

#' @export
#' @exportMethod diffExp
#' @importFrom AcidGenerics diffExp
#' @importMethodsFrom AcidSingleCell diffExp
#' @name diffExp
#' @rdname reexports
NULL

#' @export
#' @exportMethod diffExpPerCluster
#' @importFrom AcidGenerics diffExpPerCluster
#' @importMethodsFrom AcidSingleCell diffExpPerCluster
#' @name diffExpPerCluster
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics dims
#' @name dims
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics dirname
#' @name dirname
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics do.call
#' @name do.call
#' @rdname reexports
NULL

#' @export
#' @exportMethod dottedCase
#' @importFrom AcidGenerics dottedCase
#' @importMethodsFrom AcidExperiment dottedCase
#' @importMethodsFrom syntactic dottedCase
#' @name dottedCase
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors droplevels
#' @name droplevels
#' @rdname reexports
NULL

#' @export
#' @exportMethod droplevels2
#' @importFrom AcidGenerics droplevels2
#' @importMethodsFrom AcidExperiment droplevels2
#' @importMethodsFrom pipette droplevels2
#' @name droplevels2
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics duplicated
#' @name duplicated
#' @rdname reexports
NULL

#' @export
#' @exportMethod encode
#' @importFrom AcidGenerics encode
#' @importMethodsFrom AcidExperiment encode
#' @importMethodsFrom pipette encode
#' @name encode
#' @rdname reexports
NULL

#' @aliases end<-
#' @export end end<-
#' @importFrom BiocGenerics end end<-
#' @name end
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics estimateDispersions
#' @name estimateDispersions
#' @rdname reexports
NULL

#' @export
#' @exportMethod estimateSizeFactors
#' @importFrom BiocGenerics estimateSizeFactors
#' @importMethodsFrom AcidExperiment estimateSizeFactors
#' @name estimateSizeFactors
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics eval
#' @name eval
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors expand
#' @name expand
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors expand.grid
#' @name expand.grid
#' @rdname reexports
NULL

#' @export
#' @exportMethod export
#' @importFrom BiocIO export
#' @importMethodsFrom AcidExperiment export
#' @importMethodsFrom AcidGenomes export
#' @importMethodsFrom AcidSingleCell export
#' @importMethodsFrom pipette export
#' @name export
#' @rdname reexports
NULL

#' @export
#' @exportMethod factorize
#' @importFrom AcidGenerics factorize
#' @importMethodsFrom pipette factorize
#' @name factorize
#' @rdname reexports
NULL

#' @export
#' @exportMethod filterCells
#' @importFrom AcidGenerics filterCells
#' @importMethodsFrom AcidSingleCell filterCells
#' @name filterCells
#' @rdname reexports
NULL

#' @export
#' @exportMethod findMarkers
#' @importFrom AcidGenerics findMarkers
#' @importMethodsFrom AcidSingleCell findMarkers
#' @name findMarkers
#' @rdname reexports
NULL

#' @export
#' @exportMethod foldChangeToLogRatio
#' @importFrom AcidGenerics foldChangeToLogRatio
#' @importMethodsFrom AcidBase foldChangeToLogRatio
#' @name foldChangeToLogRatio
#' @rdname reexports
NULL

#' @export
#' @exportMethod fullJoin
#' @importFrom AcidGenerics fullJoin
#' @importMethodsFrom AcidPlyr fullJoin
#' @name fullJoin
#' @rdname reexports
NULL

#' @export
#' @exportMethod geneNames
#' @importFrom AcidGenerics geneNames
#' @importMethodsFrom AcidExperiment geneNames
#' @name geneNames
#' @rdname reexports
NULL

#' @aliases genome<-
#' @export genome genome<-
#' @importFrom GenomeInfoDb genome genome<-
#' @name genome
#' @rdname reexports
NULL

#' @export
#' @exportMethod geometricMean
#' @importFrom AcidGenerics geometricMean
#' @importMethodsFrom AcidBase geometricMean
#' @importMethodsFrom AcidSingleCell geometricMean
#' @name geometricMean
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics get
#' @name get
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors getListElement
#' @name getListElement
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics grep
#' @name grep
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics grepl
#' @name grepl
#' @rdname reexports
NULL

#' @export
#' @importFrom IRanges gsub
#' @name gsub
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors head
#' @name head
#' @rdname reexports
NULL

#' @export
#' @exportMethod headtail
#' @importFrom AcidGenerics headtail
#' @importMethodsFrom AcidBase headtail
#' @importMethodsFrom AcidExperiment headtail
#' @name headtail
#' @rdname reexports
NULL

#' @export
#' @exportMethod humanize
#' @importFrom AcidGenerics humanize
#' @importMethodsFrom AcidExperiment humanize
#' @name humanize
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocIO import
#' @importMethodsFrom pipette import
#' @name import
#' @rdname reexports
NULL

#' @export
#' @exportMethod innerJoin
#' @importFrom AcidGenerics innerJoin
#' @importMethodsFrom AcidPlyr innerJoin
#' @name innerJoin
#' @rdname reexports
NULL

#' @export
#' @exportMethod integerCounts
#' @importFrom AcidGenerics integerCounts
#' @importMethodsFrom AcidExperiment integerCounts
#' @name integerCounts
#' @rdname reexports
NULL

#' @aliases interestingGroups<-
#' @export interestingGroups interestingGroups<-
#' @exportMethod interestingGroups interestingGroups<-
#' @importFrom AcidGenerics interestingGroups interestingGroups<-
#' @importMethodsFrom AcidExperiment interestingGroups interestingGroups<-
#' @name interestingGroups
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics intersect
#' @name intersect
#' @rdname reexports
NULL

#' @export
#' @exportMethod intersectAll
#' @importFrom AcidGenerics intersectAll
#' @importMethodsFrom AcidBase intersectAll
#' @name intersectAll
#' @rdname reexports
NULL

#' @export
#' @exportMethod intersectionMatrix
#' @importFrom AcidGenerics intersectionMatrix
#' @importMethodsFrom AcidBase intersectionMatrix
#' @name intersectionMatrix
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics is.unsorted
#' @name is.unsorted
#' @rdname reexports
NULL

#' @export
#' @exportMethod kebabCase
#' @importFrom AcidGenerics kebabCase
#' @importMethodsFrom syntactic kebabCase
#' @name kebabCase
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics lapply
#' @name lapply
#' @rdname reexports
NULL

#' @export
#' @exportMethod leftJoin
#' @importFrom AcidGenerics leftJoin
#' @importMethodsFrom AcidPlyr leftJoin
#' @name leftJoin
#' @rdname reexports
NULL

#' @export
#' @exportMethod logRatioToFoldChange
#' @importFrom AcidGenerics logRatioToFoldChange
#' @importMethodsFrom AcidBase logRatioToFoldChange
#' @name logRatioToFoldChange
#' @rdname reexports
NULL

#' @aliases logcounts<-
#' @export logcounts logcounts<-
#' @importFrom SingleCellExperiment logcounts logcounts<-
#' @name logcounts
#' @rdname reexports
NULL

#' @export
#' @exportMethod makeDimnames
#' @importFrom AcidGenerics makeDimnames
#' @importMethodsFrom syntactic makeDimnames
#' @name makeDimnames
#' @rdname reexports
NULL

#' @export
#' @exportMethod makeLabel
#' @importFrom AcidGenerics makeLabel
#' @importMethodsFrom syntactic makeLabel
#' @name makeLabel
#' @rdname reexports
NULL

#' @export
#' @exportMethod makeNames
#' @importFrom AcidGenerics makeNames
#' @importMethodsFrom syntactic makeNames
#' @name makeNames
#' @rdname reexports
NULL

#' @export
#' @exportMethod makeSampleData
#' @importFrom AcidGenerics makeSampleData
#' @importMethodsFrom AcidExperiment makeSampleData
#' @name makeSampleData
#' @rdname reexports
NULL

#' @export
#' @exportMethod makeTitle
#' @importFrom AcidGenerics makeTitle
#' @importMethodsFrom syntactic makeTitle
#' @name makeTitle
#' @rdname reexports
NULL

#' @export
#' @exportMethod makeWords
#' @importFrom AcidGenerics makeWords
#' @importMethodsFrom syntactic makeWords
#' @name makeWords
#' @rdname reexports
NULL

#' @export
#' @exportMethod mapGenesToIDs
#' @importFrom AcidGenerics mapGenesToIDs
#' @importMethodsFrom AcidExperiment mapGenesToIDs
#' @name mapGenesToIDs
#' @rdname reexports
NULL

#' @export
#' @exportMethod mapGenesToRownames
#' @importFrom AcidGenerics mapGenesToRownames
#' @importMethodsFrom AcidExperiment mapGenesToRownames
#' @name mapGenesToRownames
#' @rdname reexports
NULL

#' @export
#' @exportMethod mapGenesToSymbols
#' @importFrom AcidGenerics mapGenesToSymbols
#' @importMethodsFrom AcidExperiment mapGenesToSymbols
#' @name mapGenesToSymbols
#' @rdname reexports
NULL

#' @export
#' @exportMethod mapToDataFrame
#' @importFrom AcidGenerics mapToDataFrame
#' @importMethodsFrom AcidPlyr mapToDataFrame
#' @name mapToDataFrame
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics mapply
#' @name mapply
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics match
#' @name match
#' @rdname reexports
NULL

#' @export
#' @exportMethod matchSampleColumn
#' @importFrom AcidGenerics matchSampleColumn
#' @importMethodsFrom AcidExperiment matchSampleColumn
#' @name matchSampleColumn
#' @rdname reexports
NULL

#' @aliases mcols<-
#' @export mcols mcols<-
#' @importFrom S4Vectors mcols mcols<-
#' @name mcols
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics mean
#' @name mean
#' @rdname reexports
NULL

#' @export
#' @importFrom IRanges median
#' @name median
#' @rdname reexports
NULL

#' @export
#' @exportMethod melt
#' @importFrom AcidGenerics melt
#' @importMethodsFrom AcidExperiment melt
#' @importMethodsFrom AcidPlyr melt
#' @importMethodsFrom AcidSingleCell melt
#' @name melt
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors merge
#' @name merge
#' @rdname reexports
NULL

#' @aliases metadata<-
#' @export metadata metadata<-
#' @importFrom S4Vectors metadata metadata<-
#' @name metadata
#' @rdname reexports
NULL

#' @aliases metadata2<-
#' @export metadata2 metadata2<-
#' @exportMethod metadata2 metadata2<-
#' @importFrom AcidGenerics metadata2 metadata2<-
#' @importMethodsFrom pipette metadata2 metadata2<-
#' @name metadata2
#' @rdname reexports
NULL

#' @export
#' @exportMethod metrics
#' @importFrom AcidGenerics metrics
#' @importMethodsFrom AcidExperiment metrics
#' @importMethodsFrom AcidSingleCell metrics
#' @name metrics
#' @rdname reexports
NULL

#' @export
#' @exportMethod metricsPerSample
#' @importFrom AcidGenerics metricsPerSample
#' @importMethodsFrom AcidSingleCell metricsPerSample
#' @name metricsPerSample
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics mget
#' @name mget
#' @rdname reexports
NULL

#' @export
#' @exportMethod mutateAll
#' @importFrom AcidGenerics mutateAll
#' @importMethodsFrom AcidPlyr mutateAll
#' @name mutateAll
#' @rdname reexports
NULL

#' @export
#' @exportMethod mutateAt
#' @importFrom AcidGenerics mutateAt
#' @importMethodsFrom AcidPlyr mutateAt
#' @name mutateAt
#' @rdname reexports
NULL

#' @export
#' @exportMethod mutateIf
#' @importFrom AcidGenerics mutateIf
#' @importMethodsFrom AcidPlyr mutateIf
#' @name mutateIf
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors na.omit
#' @name na.omit
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics ncol
#' @name ncol
#' @rdname reexports
NULL

#' @export
#' @exportMethod nonzeroRowsAndCols
#' @importFrom AcidGenerics nonzeroRowsAndCols
#' @importMethodsFrom AcidExperiment nonzeroRowsAndCols
#' @name nonzeroRowsAndCols
#' @rdname reexports
NULL

#' @export
#' @exportMethod normalize
#' @importFrom BiocGenerics normalize
#' @importMethodsFrom AcidSingleCell normalize
#' @name normalize
#' @rdname reexports
NULL

#' @aliases normcounts<-
#' @export normcounts normcounts<-
#' @importFrom SingleCellExperiment normcounts normcounts<-
#' @name normcounts
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics nrow
#' @name nrow
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics order
#' @name order
#' @rdname reexports
NULL

#' @aliases organism<-
#' @export
#' @exportMethod organism organism<-
#' @importFrom BiocGenerics organism organism<-
#' @importMethodsFrom AcidExperiment organism
#' @importMethodsFrom AcidGenomes organism organism<-
#' @name organism
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics paste
#' @name paste
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics pmax
#' @name pmax
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics pmax.int
#' @name pmax.int
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics pmin
#' @name pmin
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics pmin.int
#' @name pmin.int
#' @rdname reexports
NULL

#' @export pos
#' @importFrom BiocGenerics pos
#' @name pos
#' @rdname reexports
NULL

#' @export
#' @importFrom IRanges quantile
#' @name quantile
#' @rdname reexports
NULL

#' @export
#' @importFrom IRanges ranges
#' @name ranges
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics rank
#' @name rank
#' @rdname reexports
NULL

#' @export
#' @exportMethod rankedMatrix
#' @importFrom AcidGenerics rankedMatrix
#' @importMethodsFrom AcidBase rankedMatrix
#' @name rankedMatrix
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics rbind
#' @name rbind
#' @rdname reexports
NULL

#' @export
#' @exportMethod rbindToDataFrame
#' @importFrom AcidGenerics rbindToDataFrame
#' @importMethodsFrom AcidPlyr rbindToDataFrame
#' @name rbindToDataFrame
#' @rdname reexports
NULL

#' @aliases reducedDim<-
#' @export reducedDim reducedDim<-
#' @importFrom SingleCellExperiment reducedDim reducedDim<-
#' @name reducedDim
#' @rdname reexports
NULL

#' @aliases reducedDimNames<-
#' @export reducedDimNames reducedDimNames<-
#' @importFrom SingleCellExperiment reducedDimNames reducedDimNames<-
#' @name reducedDimNames
#' @rdname reexports
NULL

#' @aliases reducedDims<-
#' @export reducedDims reducedDims<-
#' @importFrom SingleCellExperiment reducedDims reducedDims<-
#' @name reducedDims
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics relist
#' @name relist
#' @rdname reexports
NULL

#' @export
#' @exportMethod removeNA
#' @importFrom AcidGenerics removeNA
#' @importMethodsFrom pipette removeNA
#' @name removeNA
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics rep.int
#' @name rep.int
#' @rdname reexports
NULL

#' @export
#' @exportMethod rightJoin
#' @importFrom AcidGenerics rightJoin
#' @importMethodsFrom AcidPlyr rightJoin
#' @name rightJoin
#' @rdname reexports
NULL

#' @aliases rowData<-
#' @export rowData rowData<-
#' @importFrom SummarizedExperiment rowData rowData<-
#' @name rowData
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics rowMeans
#' @name rowMeans
#' @rdname reexports
NULL

#' @aliases rowRanges<-
#' @export rowRanges rowRanges<-
#' @importFrom SummarizedExperiment rowRanges rowRanges<-
#' @name rowRanges
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics rowSums
#' @name rowSums
#' @rdname reexports
NULL

#' @aliases rownames<-
#' @export rownames rownames<-
#' @importFrom BiocGenerics rownames rownames<-
#' @name rownames
#' @rdname reexports
NULL

#' @aliases sampleData<-
#' @export sampleData sampleData<-
#' @exportMethod sampleData sampleData<-
#' @importFrom AcidGenerics sampleData sampleData<-
#' @importMethodsFrom AcidExperiment sampleData sampleData<-
#' @importMethodsFrom AcidSingleCell sampleData sampleData<-
#' @name sampleData
#' @rdname reexports
NULL

#' @aliases sampleNames<-
#' @export sampleNames sampleNames<-
#' @exportMethod sampleNames sampleNames<-
#' @importFrom Biobase sampleNames sampleNames<-
#' @importMethodsFrom AcidExperiment sampleNames sampleNames<-
#' @name sampleNames
#' @rdname reexports
NULL

#' @export
#' @exportMethod sanitizeNA
#' @importFrom AcidGenerics sanitizeNA
#' @importMethodsFrom pipette sanitizeNA
#' @name sanitizeNA
#' @rdname reexports
NULL

#' @export
#' @exportMethod sanitizePercent
#' @importFrom AcidGenerics sanitizePercent
#' @importMethodsFrom pipette sanitizePercent
#' @name sanitizePercent
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics sapply
#' @name sapply
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics sd
#' @name sd
#' @rdname reexports
NULL

#' @export
#' @exportMethod selectIf
#' @importFrom AcidGenerics selectIf
#' @importMethodsFrom AcidPlyr selectIf
#' @name selectIf
#' @rdname reexports
NULL

#' @export
#' @exportMethod selectSamples
#' @importFrom AcidGenerics selectSamples
#' @importMethodsFrom AcidExperiment selectSamples
#' @importMethodsFrom AcidSingleCell selectSamples
#' @name selectSamples
#' @rdname reexports
NULL

#' @export
#' @exportMethod sem
#' @importFrom AcidGenerics sem
#' @importMethodsFrom AcidBase sem
#' @name sem
#' @rdname reexports
NULL

#' @export
#' @exportMethod semiJoin
#' @importFrom AcidGenerics semiJoin
#' @importMethodsFrom AcidPlyr semiJoin
#' @name semiJoin
#' @rdname reexports
NULL

#' @export
#' @exportMethod sentenceCase
#' @importFrom AcidGenerics sentenceCase
#' @importMethodsFrom syntactic sentenceCase
#' @name sentenceCase
#' @rdname reexports
NULL

#' @aliases seqinfo<-
#' @export seqinfo seqinfo<-
#' @importFrom GenomeInfoDb seqinfo seqinfo<-
#' @name seqinfo
#' @rdname reexports
NULL

#' @aliases seqlevels<-
#' @export seqlevels seqlevels<-
#' @importFrom GenomeInfoDb seqlevels seqlevels<-
#' @name seqlevels
#' @rdname reexports
NULL

#' @aliases seqnames<-
#' @export seqnames seqnames<-
#' @importFrom GenomeInfoDb seqnames seqnames<-
#' @name seqnames
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics setdiff
#' @name setdiff
#' @rdname reexports
NULL

#' @export
#' @exportMethod showHeader
#' @importFrom AcidGenerics showHeader
#' @importMethodsFrom AcidBase showHeader
#' @name showHeader
#' @rdname reexports
NULL

#' @aliases sizeFactors<-
#' @export sizeFactors sizeFactors<-
#' @exportMethod sizeFactors sizeFactors<-
#' @importFrom BiocGenerics sizeFactors sizeFactors<-
#' @importMethodsFrom AcidExperiment sizeFactors sizeFactors<-
#' @name sizeFactors
#' @rdname reexports
NULL

#' @export
#' @exportMethod snakeCase
#' @importFrom AcidGenerics snakeCase
#' @importMethodsFrom AcidExperiment snakeCase
#' @importMethodsFrom syntactic snakeCase
#' @name snakeCase
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics sort
#' @name sort
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors split
#' @name split
#' @rdname reexports
NULL

#' @export
#' @exportMethod splitByLevel
#' @importFrom AcidGenerics splitByLevel
#' @importMethodsFrom AcidPlyr splitByLevel
#' @name splitByLevel
#' @rdname reexports
NULL

#' @aliases start<-
#' @export start start<-
#' @importFrom BiocGenerics start start<-
#' @name start
#' @rdname reexports
NULL

#' @aliases strand<-
#' @export strand strand<-
#' @importFrom BiocGenerics strand strand<-
#' @name strand
#' @rdname reexports
NULL

#' @export
#' @exportMethod stripGeneVersions
#' @importFrom AcidGenerics stripGeneVersions
#' @importMethodsFrom AcidExperiment stripGeneVersions
#' @importMethodsFrom AcidGenomes stripGeneVersions
#' @name stripGeneVersions
#' @rdname reexports
NULL

#' @export
#' @exportMethod stripTranscriptVersions
#' @importFrom AcidGenerics stripTranscriptVersions
#' @importMethodsFrom AcidExperiment stripTranscriptVersions
#' @importMethodsFrom AcidGenomes stripTranscriptVersions
#' @name stripTranscriptVersions
#' @rdname reexports
NULL

#' @export
#' @importFrom IRanges sub
#' @name sub
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics subset
#' @name subset
#' @rdname reexports
NULL

#' @export
#' @exportMethod subsetPerSample
#' @importFrom AcidGenerics subsetPerSample
#' @importMethodsFrom AcidSingleCell subsetPerSample
#' @name subsetPerSample
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors summary
#' @name summary
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics t
#' @name t
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics table
#' @name table
#' @rdname reexports
NULL

#' @export
#' @importFrom S4Vectors tail
#' @name tail
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics tapply
#' @name tapply
#' @rdname reexports
NULL

#' @export
#' @exportMethod topCellsPerSample
#' @importFrom AcidGenerics subsetPerSample
#' @importMethodsFrom AcidSingleCell topCellsPerSample
#' @name topCellsPerSample
#' @rdname reexports
NULL

#' @export
#' @importFrom IRanges trim
#' @name trim
#' @rdname reexports
NULL

#' @export
#' @exportMethod tpm
#' @importFrom AcidGenerics tpm
#' @importMethodsFrom AcidExperiment tpm
#' @name tpm
#' @rdname reexports
NULL

#' @export
#' @exportMethod transmuteAt
#' @importFrom AcidGenerics transmuteAt
#' @importMethodsFrom AcidPlyr transmuteAt
#' @name transmuteAt
#' @rdname reexports
NULL

#' @export
#' @exportMethod transmuteIf
#' @importFrom AcidGenerics transmuteIf
#' @importMethodsFrom AcidPlyr transmuteIf
#' @name transmuteIf
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics union
#' @name union
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics unique
#' @name unique
#' @rdname reexports
NULL

#' @export
#' @exportMethod uniteInterestingGroups
#' @importFrom AcidGenerics uniteInterestingGroups
#' @importMethodsFrom AcidExperiment uniteInterestingGroups
#' @name uniteInterestingGroups
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics unlist
#' @name unlist
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics unsplit
#' @name unsplit
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics updateObject
#' @name updateObject
#' @rdname reexports
NULL

#' @export
#' @exportMethod upperCamelCase
#' @importFrom AcidGenerics upperCamelCase
#' @importMethodsFrom AcidExperiment upperCamelCase
#' @importMethodsFrom syntactic upperCamelCase
#' @name upperCamelCase
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics var
#' @name var
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics weights
#' @name weights
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics which
#' @name which
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics which.max
#' @name which.max
#' @rdname reexports
NULL

#' @export
#' @importFrom BiocGenerics which.min
#' @name which.min
#' @rdname reexports
NULL

#' @aliases width<-
#' @export width width<-
#' @importFrom BiocGenerics width width<-
#' @name width
#' @rdname reexports
NULL

#' @export
#' @exportMethod zerosVsDepth
#' @importFrom AcidGenerics zerosVsDepth
#' @importMethodsFrom AcidSingleCell zerosVsDepth
#' @name zerosVsDepth
#' @rdname reexports
NULL
