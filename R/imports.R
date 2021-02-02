## FIXME ANY MATRIX METHODS NEED TO GO IN ACIDSINGLECELL.



#' @importClassesFrom AcidGenerics AsIs
#' @importClassesFrom GenomicRanges GRanges GRangesList
#' @importClassesFrom IRanges SimpleDataFrameList
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors Annotated DataFrame List Vector
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL



#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom SummarizedExperiment coerce
#' @importMethodsFrom pipette as.data.frame coerce
NULL



#' @importFrom AcidBase appendToBody bapply compress getNameInParent
#'   matchArgsToDoCall methodFormals methodFunction printString
#'   requireNamespaces
#' @importFrom AcidGenerics %in% aggregate as.data.frame as.list colSums
#'   complete.cases cor decode do.call expand expand.grid head lapply match
#'   mcols mcols<- merge metadata metadata<- na.omit order rowSums setdiff split
#'   summary t tail unlist
#' @importFrom Biostrings reverseComplement
#' @importFrom IRanges DataFrameList IRanges SplitDataFrameList unsplit
#' @importFrom Matrix fac2sparse
#' @importFrom S4Vectors DataFrame List Rle SimpleList
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom SummarizedExperiment SummarizedExperiment assay assay<-
#'   assayNames assayNames<- assays assays<- colData colData<- rowData rowData<-
#'   rowRanges rowRanges<-
#' @importFrom cli cli_alert cli_alert_info cli_alert_success cli_alert_warning
#'   cli_div cli_dl cli_end cli_li cli_text cli_ul cli_verbatim
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
#' @importFrom utils capture.output data getFromNamespace installed.packages
#'   packageName packageVersion
NULL



## This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL
