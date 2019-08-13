## Conflicts with BiocGenerics:
## #' @importMethodsFrom Matrix colSums rowMeans rowSums t



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
#'   rowSums setdiff t
#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedMatrixStats colSums2 rowSums2
#' @importFrom GenomeInfoDb seqnames
#' @importFrom IRanges DataFrameList
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom S4Vectors DataFrame List Rle SimpleList %in% aggregate cor decode
#'   expand head lapply mcols mcols<- merge metadata metadata<- na.omit split
#'   summary tail
#' @importFrom SingleCellExperiment SingleCellExperiment isSpike<- spikeNames
#' @importFrom SummarizedExperiment SummarizedExperiment assay assay<-
#'   assayNames assayNames<- assays assays<- colData colData<- rowData rowData<-
#'   rowRanges rowRanges<-
#' @importFrom dplyr arrange bind_rows desc filter group_by left_join mutate
#'   mutate_all mutate_at mutate_if pull rename select select_if slice
#'   summarise_all top_n ungroup
#' @importFrom goalie allAreMatchingRegex appendToBody areDisjointSets
#'   areIntersectingSets areSameLength areSetEqual assert bapply containsAURL
#'   false getNameInParent hasColnames hasCols hasDims hasInternet hasLength
#'   hasMetrics hasNames hasNoDuplicates hasNonZeroRowsAndCols hasRows
#'   hasRownames hasSubset hasUniqueCols hasValidDimnames hasValidNames
#'   isADirectory isAFile isAlpha isAny isCharacter isFlag isGGScale
#'   isGreaterThanOrEqualTo isHeaderLevel isHexColorFunction isInClosedRange
#'   isInLeftOpenRange isInRange isInt isIntegerish isMatchingRegex
#'   isNonNegative isNotMatchingRegex isNumber isPositive isScalar isString
#'   isSubset isSuperset matchesUniqueGeneNames methodFunction validNames
#'   validate validateClasses
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom magrittr %>% set_colnames set_rownames
#' @importFrom matrixStats colVars rowVars
#' @importFrom methods as coerce formalArgs getGeneric getMethod is isGeneric
#'   new selectMethod setAs setClass setGeneric setMethod setReplaceMethod
#'   setOldClass setValidity show signature slot slotNames validObject .hasSlot
#' @importFrom purrr map
#' @importFrom readr cols read_lines read_tsv
#' @importFrom reshape2 melt
#' @importFrom rlang !! !!! := UQ sym syms
#' @importFrom scales percent
#' @importFrom sessioninfo session_info
#' @importFrom stringr str_dup str_extract str_length str_match str_pad
#'   str_replace_all str_subset str_trunc
#' @importFrom tidyr separate
#' @importFrom tidyselect everything matches
#' @importFrom tibble as_tibble column_to_rownames tibble
#' @importFrom transformer as_tibble
#' @importFrom utils capture.output data getFromNamespace globalVariables
#'   installed.packages packageVersion
NULL
