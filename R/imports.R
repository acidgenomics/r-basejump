## FIXME ANY MATRIX METHODS NEED TO GO IN ACIDSINGLECELL.



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
#' @importFrom IRanges DataFrameList IRanges SplitDataFrameList unsplit
#' @importFrom S4Vectors DataFrame List Rle SimpleList
#' @importFrom SummarizedExperiment SummarizedExperiment assay assay<-
#'   assayNames assayNames<- assays assays<- colData colData<- rowData rowData<-
#'   rowRanges rowRanges<-
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
#' @importFrom methods as coerce formalArgs getGeneric getMethod is isGeneric
#'   new selectMethod setAs setClass setClassUnion setGeneric setMethod
#'   setReplaceMethod setOldClass setValidity signature slot slotNames
#'   validObject .hasSlot
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
