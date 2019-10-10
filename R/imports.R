## > #' @importFrom DelayedArray DelayedArray
## > #' @importFrom DelayedMatrixStats colSums2 rowSums2



#' @importClassesFrom GenomicRanges GRanges GRangesList
#' @importClassesFrom IRanges SimpleDataFrameList
#' @importClassesFrom Matrix sparseMatrix
#' @importClassesFrom S4Vectors Annotated DataFrame DataTable List Vector
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importMethodsFrom S4Vectors as.data.frame as.list coerce do.call lapply
#'   match rep setdiff t unique
#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom SummarizedExperiment coerce
#' @importMethodsFrom transformer as.data.frame coerce
#'
#' @importFrom AnnotationDbi select
#' @importFrom BiocGenerics colSums rowSums unlist
#' @importFrom BiocParallel bplapply
#' @importFrom Biostrings reverseComplement
#' @importFrom GenomeInfoDb seqnames
#' @importFrom IRanges DataFrameList SplitDataFrameList unsplit
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom S4Vectors DataFrame List Rle SimpleList %in% aggregate
#'   as.data.frame as.list cor decode do.call expand expand.grid head lapply
#'   match mcols mcols<- merge metadata metadata<- na.omit order setdiff split
#'   summary t tail
#' @importFrom SingleCellExperiment SingleCellExperiment isSpike<-
#' @importFrom SummarizedExperiment SummarizedExperiment assay assay<-
#'   assayNames assayNames<- assays assays<- colData colData<- rowData rowData<-
#'   rowRanges rowRanges<-
#' @importFrom biomaRt listEnsemblArchives listMarts useMart
#' @importFrom goalie allAreAtomic allAreMatchingRegex allAreNotMatchingRegex
#'   appendToBody areDisjointSets areIntersectingSets areSameLength areSetEqual
#'   assert bapply containsAURL false getNameInParent hasColnames hasCols
#'   hasDimnames hasDims hasInternet hasLength hasMetrics hasNames
#'   hasNoDuplicates hasNonzeroRowsAndCols hasRows hasRownames hasSubset
#'   hasUniqueCols hasValidDimnames hasValidNames isADirectory isAFile isAlpha
#'   isAny isCharacter isFlag isGGScale isGreaterThanOrEqualTo isHeaderLevel
#'   isHexColorFunction isInClosedRange isInLeftOpenRange isInRange isInt
#'   isIntegerish isMatchingRegex isNonNegative isNotMatchingRegex isNumber
#'   isPositive isScalar isString isSubset isSuperset matchArgsToDoCall
#'   matchesUniqueGeneNames methodFormals methodFunction validNames validate
#'   validateClasses
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom matrixStats colVars rowVars
#' @importFrom methods as coerce formalArgs getGeneric getMethod is isGeneric
#'   new selectMethod setAs setClass setGeneric setMethod setReplaceMethod
#'   setOldClass setValidity signature slot slotNames validObject .hasSlot
#' @importFrom scales percent
#' @importFrom sessioninfo session_info
#' @importFrom stringr str_detect str_dup str_extract str_length str_match
#'   str_pad str_replace_all str_subset str_trunc
#' @importFrom transformer as_tibble
#' @importFrom utils capture.output data getFromNamespace globalVariables
#'   installed.packages packageVersion
NULL
