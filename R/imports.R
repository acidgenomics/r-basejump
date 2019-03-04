# Conflicts with BiocGenerics:
# @importMethodsFrom Matrix colSums rowMeans rowSums t

# SingleCellExperiment prior to BioC 3.8 doesn't define coercion methods.
# We're maintaiing legacy support for R 3.4 (BioC 3.5/3.6), so disable.
# @importMethodsFrom SingleCellExperiment coerce



#' @importClassesFrom GenomicRanges GRanges GRangesList
#' @importClassesFrom IRanges SimpleDataFrameList
#' @importClassesFrom Matrix sparseMatrix
#' @importClassesFrom S4Vectors DataFrame List
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importMethodsFrom S4Vectors as.data.frame as.list coerce do.call lapply
#'   match setdiff t
#' @importMethodsFrom SummarizedExperiment coerce
#' @importMethodsFrom transformer as.data.frame coerce
#'
#' @importFrom AnnotationHub AnnotationHub query snapshotDate
#' @importFrom BiocGenerics as.data.frame as.list colSums do.call lapply match
#'   rowSums setdiff t
#' @importFrom GenomeInfoDb seqnames
#' @importFrom GenomicFeatures genes makeTxDbFromGFF makeTxDbFromGRanges
#'   transcripts
#' @importFrom GenomicRanges GRanges ranges
#' @importFrom IRanges IRanges
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom RColorBrewer brewer.pal
#' @importFrom S4Vectors DataFrame List Rle %in% aggregate cor decode expand
#'   head mcols mcols<- merge metadata metadata<- na.omit split tail
#' @importFrom SingleCellExperiment SingleCellExperiment isSpike<- spikeNames
#' @importFrom SummarizedExperiment SummarizedExperiment assay assayNames
#'   assayNames<- assays assays<- colData colData<- rowData rowData<- rowRanges
#'   rowRanges<-
#' @importFrom cowplot plot_grid
#' @importFrom dplyr arrange bind_rows case_when desc filter funs group_by
#'   left_join mutate mutate_all mutate_at mutate_if n pull rename select
#'   select_if slice summarise summarise_all top_n ungroup
#' @importFrom ensembldb ensemblVersion
#' @importFrom ggplot2 aes coord_fixed coord_flip element_blank element_line
#'   element_rect element_text expand_limits facet_wrap geom_bar geom_boxplot
#'   geom_density geom_hline geom_jitter geom_label geom_point geom_violin
#'   geom_vline ggplot guides labs position_jitterdodge scale_x_continuous
#'   scale_y_continuous stat_ecdf stat_summary theme theme_linedraw
#' @importFrom ggrepel geom_label_repel
#' @importFrom goalie allAreHexColors areDisjointSets areIntersectingSets
#'   areSameLength areSetEqual assert false getNameInParent hasDims
#'   hasDuplicates hasInternet hasLength hasNames hasNoDuplicates
#'   hasNonZeroRowsAndCols hasRows hasRownames hasUniqueCols hasValidDimnames
#'   isADirectory  isAny isCharacter isFlag isGGScale isHeaderLevel
#'   isHexColorFunction isInClosedRange isInRange isInt isMatchingRegex
#'   isNonNegative isNotMatchingRegex isNumber isPositive isScalar isString
#'   isSubset isSuperset matchesUniqueGeneNames validNames validate
#'   validateClasses
#' @importFrom grDevices colorRampPalette
#' @importFrom grid arrow unit
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom magrittr %>% set_colnames set_rownames
#' @importFrom matrixStats colVars rowVars
#' @importFrom methods as coerce formalArgs getGeneric getMethod is isGeneric
#'   new selectMethod setAs setClass setGeneric setMethod setOldClass show
#'   signature slot slotNames validObject .hasSlot
#' @importFrom pheatmap pheatmap
#' @importFrom purrr map
#' @importFrom readr cols read_lines read_tsv
#' @importFrom reshape2 melt
#' @importFrom rlang !! !!! := UQ sym syms
#' @importFrom sessioninfo session_info
#' @importFrom stats as.formula dist hclust prcomp quantile
#' @importFrom stringr str_dup str_extract str_length str_match str_pad
#'   str_replace_all str_subset str_trunc
#' @importFrom tidyr gather separate
#' @importFrom tidyselect everything matches
#' @importFrom tibble as_tibble column_to_rownames tibble
#' @importFrom transformer as_tibble
#' @importFrom utils capture.output data getFromNamespace globalVariables
#'   installed.packages packageVersion
NULL
