## FIXME ANY MATRIX METHODS NEED TO GO IN ACIDSINGLECELL.
## FIXME NEED TO REEXPORT PIPETTE AS.DATA.FRAME HERE.



#' @importClassesFrom GenomicRanges GRanges
#' @importClassesFrom IRanges IRanges
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors Annotated DataFrame List Vector
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL



## FIXME REDUCE THE NUMBER OF GOALIE IMPORTS.

#' @importFrom AcidBase requireNamespaces
#' @importFrom AcidGenerics %in% as.data.frame expand.grid lapply mcols mcols<-
#'   metadata metadata<- unlist
#' @importFrom SummarizedExperiment SummarizedExperiment assay assay<-
#'   assayNames assayNames<- assays assays<- colData colData<- rowData rowData<-
#'   rowRanges rowRanges<-
#' @importFrom goalie areSameLength assert hasColnames hasDimnames hasLength
#'   hasNames hasRownames
#'
#'   hasUniqueCols hasValidDimnames hasValidNames
#'   isADirectory isAFile isAURL isAlpha isAny isCharacter isFlag isGGScale
#'   isGreaterThanOrEqualTo isHeaderLevel isHexColorFunction isInClosedRange
#'   isInLeftOpenRange isInRange isInstalled isInt isIntegerish isMatchingRegex
#'   isNonNegative isNotMatchingRegex isNumber isPositive isScalar isString
#'   isSubset isSuperset matchesUniqueGeneNames validNames validate
#'   validateClasses
#' @importFrom methods as coerce getMethod is setAs setMethod signature slot
#'   slotNames validObject .hasSlot
#' @importFrom stringr str_dup str_length str_pad
#' @importFrom utils packageName packageVersion
NULL



#' @importFrom methods coerce
#' @importMethodsFrom SummarizedExperiment coerce
#' @importMethodsFrom pipette coerce
#' @exportMethod coerce
NULL
