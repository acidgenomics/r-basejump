## > #' @importFrom DelayedArray DelayedArray
## > #' @importFrom DelayedMatrixStats colSums2 rowSums2



#' @importClassesFrom GenomicRanges GRanges GRangesList
#' @importClassesFrom IRanges SimpleDataFrameList
#' @importClassesFrom Matrix sparseMatrix
#' @importClassesFrom S4Vectors Annotated DataFrame DataTable List Vector
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL



#' @importMethodsFrom S4Vectors as.data.frame as.list coerce do.call lapply
#'   match rep setdiff t unique
#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom SummarizedExperiment coerce
#' @importMethodsFrom pipette as.data.frame coerce
NULL



#' @importFrom AnnotationHub AnnotationHub query snapshotDate
#' @importFrom AnnotationDbi select
#' @importFrom BiocGenerics colSums organism rowSums unlist
#' @importFrom BiocParallel bplapply
#' @importFrom Biostrings reverseComplement
#' @importFrom GenomeInfoDb seqnames
#' @importFrom GenomicFeatures genes makeTxDbFromGFF makeTxDbFromGRanges
#'   transcripts
#' @importFrom GenomicRanges GRanges ranges
#' @importFrom IRanges DataFrameList IRanges SplitDataFrameList unsplit
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom RCurl getURL url.exists
#' @importFrom R.utils gzip
#' @importFrom S4Vectors DataFrame List Rle SimpleList %in% aggregate
#'   as.data.frame as.list cor decode do.call expand expand.grid head lapply
#'   match mcols mcols<- merge metadata metadata<- na.omit order setdiff split
#'   summary t tail
#' @importFrom SingleCellExperiment SingleCellExperiment isSpike<-
#' @importFrom SummarizedExperiment SummarizedExperiment assay assay<-
#'   assayNames assayNames<- assays assays<- colData colData<- rowData rowData<-
#'   rowRanges rowRanges<-
#' @importFrom acidbase appendToBody bapply getNameInParent matchArgsToDoCall
#'    methodFormals methodFunction printString
#' @importFrom biomaRt listEnsemblArchives listMarts useMart
#' @importFrom ensembldb ensemblVersion
#' @importFrom goalie allAreAtomic allAreMatchingRegex allAreNotMatchingRegex
#'   areDisjointSets areIntersectingSets areSameLength areSetEqual assert false
#'   hasColnames hasCols hasDimnames hasDims hasDuplicates hasInternet hasLength
#'   hasMetrics hasNames hasNoDuplicates hasNonzeroRowsAndCols hasRows
#'   hasRownames hasSubset hasUniqueCols hasValidDimnames hasValidNames
#'   isADirectory isAFile isAURL isAlpha isAny isCharacter isFlag isGGScale
#'   isGreaterThanOrEqualTo isHeaderLevel isHexColorFunction isInClosedRange
#'   isInLeftOpenRange isInRange isInt isIntegerish isMatchingRegex
#'   isNonNegative isNotMatchingRegex isNumber isPositive isScalar isString
#'   isSubset isSuperset matchesUniqueGeneNames validNames validate
#'   validateClasses
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom matrixStats colVars rowVars
#' @importFrom methods as coerce formalArgs getGeneric getMethod is isGeneric
#'   new selectMethod setAs setClass setClassUnion setGeneric setMethod
#'   setReplaceMethod setOldClass setValidity signature slot slotNames
#'   validObject .hasSlot
#' @importFrom scales percent
#' @importFrom sessioninfo session_info
#' @importFrom stringr boundary str_detect str_dup str_extract str_length
#'   str_match str_pad str_replace_all str_split str_split_fixed str_subset
#'   str_trunc
#' @importFrom pipette as_tibble
#' @importFrom utils capture.output data getFromNamespace globalVariables
#'   installed.packages packageVersion
NULL



## This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL
