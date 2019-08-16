## FIXME Reduce the number of tidyverse imports.

## Conflicts with BiocGenerics:
## #' @importMethodsFrom Matrix colSums rowMeans rowSums t
## #' @importFrom tidyr expand

## FIXME Switch to data.table instead of readr here?

## FIXME Reduce these dependencies:
## #' @importFrom dplyr arrange bind_rows desc filter group_by mutate pull rename
## #'   select slice summarise_all top_n ungroup
## #' @importFrom magrittr set_colnames set_rownames
## #' @importFrom readr cols read_lines read_tsv
## #' @importFrom rlang !! !!! := UQ sym syms
## #' @importFrom tibble column_to_rownames tibble
## #' @importFrom tidyr separate
## #' @importFrom tidyselect everything matches
## #' @importFrom transformer as_tibble



#' @importClassesFrom GenomicRanges GRanges GRangesList
#' @importClassesFrom IRanges SimpleDataFrameList
#' @importClassesFrom Matrix sparseMatrix
#' @importClassesFrom S4Vectors Annotated DataFrame DataTable List Vector
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importMethodsFrom S4Vectors as.data.frame as.list coerce do.call lapply
#'   match setdiff t
#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom SummarizedExperiment coerce
#' @importMethodsFrom transformer as.data.frame coerce
#'
#' @importFrom BiocGenerics as.data.frame as.list colSums do.call lapply match
#'   rowSums setdiff t unlist
#' @importFrom BiocParallel bplapply
#' @importFrom Biostrings reverseComplement
#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedMatrixStats colSums2 rowSums2
#' @importFrom GenomeInfoDb seqnames
#' @importFrom IRanges DataFrameList SplitDataFrameList
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom S4Vectors DataFrame List Rle SimpleList %in% aggregate cor decode
#'   expand head lapply mcols mcols<- merge metadata metadata<- na.omit order
#'   split summary tail
#' @importFrom SingleCellExperiment SingleCellExperiment isSpike<- spikeNames
#' @importFrom SummarizedExperiment SummarizedExperiment assay assay<-
#'   assayNames assayNames<- assays assays<- colData colData<- rowData rowData<-
#'   rowRanges rowRanges<-
#' @importFrom goalie allAreAtomic allAreMatchingRegex allAreNotMatchingRegex
#'   appendToBody areDisjointSets areIntersectingSets areSameLength areSetEqual
#'   assert bapply containsAURL false getNameInParent hasColnames hasCols
#'   hasDims hasInternet hasLength hasMetrics hasNames hasNoDuplicates
#'   hasNonZeroRowsAndCols hasRows hasRownames hasSubset hasUniqueCols
#'   hasValidDimnames hasValidNames isADirectory isAFile isAlpha isAny
#'   isCharacter isFlag isGGScale isGreaterThanOrEqualTo isHeaderLevel
#'   isHexColorFunction isInClosedRange isInLeftOpenRange isInRange isInt
#'   isIntegerish isMatchingRegex isNonNegative isNotMatchingRegex isNumber
#'   isPositive isScalar isString isSubset isSuperset matchesUniqueGeneNames
#'   methodFormals methodFunction validNames validate validateClasses
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom matrixStats colVars rowVars
#' @importFrom methods as coerce formalArgs getGeneric getMethod is isGeneric
#'   new selectMethod setAs setClass setGeneric setMethod setReplaceMethod
#'   setOldClass setValidity show signature slot slotNames validObject .hasSlot
#' @importFrom reshape2 melt
#' @importFrom scales percent
#' @importFrom sessioninfo session_info
#' @importFrom stringr str_dup str_extract str_length str_match str_pad
#'   str_replace_all str_subset str_trunc
#' @importFrom utils capture.output data getFromNamespace globalVariables
#'   installed.packages packageVersion
NULL
