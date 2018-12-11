#' basejump
#'
#' Toolkit for bioinformatics and R package development.
#'
#' @keywords internal
#'
#' @importClassesFrom GenomicRanges GRanges GRangesList
#' @importClassesFrom IRanges SimpleDataFrameList
#' @importClassesFrom Matrix sparseMatrix
#' @importClassesFrom S4Vectors DataFrame List
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importMethodsFrom S4Vectors as.data.frame as.list coerce do.call lapply
#'   match setdiff t
#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom SummarizedExperiment coerce
#'
#' @importFrom AnnotationHub AnnotationHub query snapshotDate
#' @importFrom Biobase sampleNames sampleNames<-
#' @importFrom BiocGenerics as.data.frame as.list colSums do.call lapply match
#'   rowMeans rowSums setdiff t
#' @importFrom GenomicFeatures genes transcripts
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom Matrix readMM readMM writeMM
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom R.utils gzip
#' @importFrom RColorBrewer brewer.pal
#' @importFrom RCurl getURL url.exists
#' @importFrom S4Vectors %in% List Rle aggregate complete.cases cor decode
#'   expand head mcols mcols<- metadata metadata<- na.omit tail
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom cowplot plot_grid
#' @importFrom data.table as.data.table fread
#' @importFrom dplyr arrange bind_rows case_when desc everything filter funs
#'   group_by left_join mutate mutate_all mutate_at mutate_if n pull rename
#'   select select_if slice summarise summarise_all top_n ungroup
#' @importFrom ensembldb ensemblVersion organism
#' @importFrom ggplot2 aes coord_fixed coord_flip element_blank element_line
#'   element_rect element_text expand_limits facet_wrap geom_bar geom_boxplot
#'   geom_density geom_hline geom_jitter geom_label geom_point geom_violin
#'   geom_vline ggplot guides labs position_jitterdodge scale_x_continuous
#'   scale_y_continuous stat_ecdf stat_summary theme theme_linedraw
#' @importFrom ggrepel geom_label_repel
#' @importFrom goalie areDisjointSets areExisting areIntersectingSets
#'   areNonExisting areSameLength areSetEqual assert containsAURL
#'   containsHeaderLevel containsHexColors containsURL formalCompress hasAccess
#'   hasColnames hasDimnames hasCols hasDims hasInternet hasLength hasNames
#'   hasNoDuplicates hasNonZeroRowsAndCols hasRows hasRownames hasUniqueCols
#'   hasValidDimnames isADirectory isAFile isAny isCharacter isDirectory
#'   isExisting isFile isFlag isGGScale isHexColorFunction isInClosedRange
#'   isInRange isInt isMatchingRegex isNonEmpty isNonNegative isNumber
#'   isPositive isScalar isString isSubset isSuperset makeTestFunction
#'   matchesUniqueGeneNames validNames validate validateClasses
#' @importFrom grDevices colorRampPalette
#' @importFrom grid arrow unit
#' @importFrom jsonlite read_json
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom magrittr set_colnames set_rownames
#' @importFrom matrixStats colVars rowVars
#' @importFrom methods as formalArgs getGeneric getMethod is isGeneric new
#'   selectMethod setAs setClass setGeneric setMethod setOldClass setValidity
#'   show signature slot slotNames validObject .hasSlot
#' @importFrom pbapply pblapply
#' @importFrom pheatmap pheatmap
#' @importFrom purrr map
#' @importFrom readr cols read_lines read_tsv write_csv write_lines
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#' @importFrom rlang !! !!! := dots_list eval_bare sym syms UQ
#' @importFrom rtracklayer import
#' @importFrom sessioninfo session_info
#' @importFrom stats as.formula dist hclust prcomp quantile
#' @importFrom stringr regex str_dup str_extract str_length str_match str_pad
#'   str_replace str_replace_all str_replace_na str_subset str_trunc
#' @importFrom tidyr gather separate unite
#' @importFrom tidyselect everything matches starts_with
#' @importFrom tibble as_tibble column_to_rownames tibble
#' @importFrom tools Rd_db file_path_sans_ext
#' @importFrom utils capture.output data download.file getFromNamespace
#'   globalVariables installed.packages packageVersion read.delim read.table
#'   sessionInfo
#' @importFrom yaml yaml.load_file
"_PACKAGE"

# Conflicts with rtracklayer:
# @importFrom rio import

# Conflicts with BiocGenerics:
# @importMethodsFrom Matrix colSums rowMeans rowSums t

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom S4Vectors DataFrame
#' @export
S4Vectors::DataFrame

#' @importFrom SingleCellExperiment isSpike
#' @export
SingleCellExperiment::isSpike

#' @importFrom SingleCellExperiment isSpike<-
#' @export
SingleCellExperiment::`isSpike<-`

#' @importFrom SingleCellExperiment reducedDimNames
#' @export
SingleCellExperiment::reducedDimNames

#' @importFrom SingleCellExperiment reducedDims
#' @export
SingleCellExperiment::reducedDims

#' @importFrom SingleCellExperiment spikeNames
#' @export
SingleCellExperiment::spikeNames

#' @importFrom SummarizedExperiment assay
#' @export
SummarizedExperiment::assay

#' @importFrom SummarizedExperiment colData
#' @export
SummarizedExperiment::colData

#' @importFrom SummarizedExperiment colData<-
#' @export
SummarizedExperiment::`colData<-`

#' @importFrom SummarizedExperiment mcols
#' @export
SummarizedExperiment::mcols

#' @importFrom SummarizedExperiment rowRanges
#' @export
SummarizedExperiment::rowRanges

#' @importFrom SummarizedExperiment rowRanges<-
#' @export
SummarizedExperiment::`rowRanges<-`

#' @importFrom SummarizedExperiment seqnames
#' @export
SummarizedExperiment::seqnames

#' @importFrom SummarizedExperiment assayNames
#' @export
SummarizedExperiment::assayNames

#' @importFrom SummarizedExperiment assayNames<-
#' @export
SummarizedExperiment::`assayNames<-`

#' @importFrom SummarizedExperiment assays
#' @export
SummarizedExperiment::assays

#' @importFrom SummarizedExperiment assays<-
#' @export
SummarizedExperiment::`assays<-`

#' @importFrom SummarizedExperiment colData
#' @export
SummarizedExperiment::colData

#' @importFrom SummarizedExperiment colData<-
#' @export
SummarizedExperiment::`colData<-`

#' @importFrom SummarizedExperiment rowData
#' @export
SummarizedExperiment::rowData

#' @importFrom SummarizedExperiment rowData<-
#' @export
SummarizedExperiment::`rowData<-`

#' @importFrom SummarizedExperiment rowRanges
#' @export
SummarizedExperiment::rowRanges

#' @importFrom SummarizedExperiment rowRanges<-
#' @export
SummarizedExperiment::`rowRanges<-`
