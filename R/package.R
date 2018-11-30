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
#' @importFrom S4Vectors %in% DataFrame List Rle aggregate complete.cases cor
#'   decode expand head mcols mcols<- metadata metadata<- na.omit tail
#' @importFrom SingleCellExperiment SingleCellExperiment isSpike<-
#'   reducedDimNames reducedDims spikeNames
#' @importFrom SummarizedExperiment SummarizedExperiment assay assayNames
#'   assayNames<- assays assays<- colData colData<- rowData rowData<- rowRanges
#'   rowRanges<-
#' @importFrom assertive.base assert_all_are_not_na assert_all_are_true
#'   assert_any_are_true assert_are_identical assert_is_identical_to_na
#'   is_not_na
#' @importFrom assertive.code assert_all_are_existing is_existing
#' @importFrom assertive.data assert_all_are_hex_colors is_hex_color
#' @importFrom assertive.files assert_all_are_dirs assert_all_are_existing_files
#'   assert_all_are_non_empty_files
#' @importFrom assertive.numbers assert_all_are_greater_than
#'   assert_all_are_greater_than_or_equal_to assert_all_are_in_closed_range
#'   assert_all_are_in_open_range assert_all_are_in_range
#'   assert_all_are_non_negative assert_all_are_positive is_positive
#' @importFrom assertive.properties assert_are_same_length assert_has_colnames
#'   assert_has_cols assert_has_dimnames assert_has_dims assert_has_names
#'   assert_has_no_duplicates assert_has_rownames assert_has_rows
#'   assert_is_atomic assert_is_empty assert_is_non_empty assert_is_not_null
#'   assert_is_null assert_is_of_length assert_is_scalar assert_is_vector
#'   has_colnames has_dimnames has_dims has_names has_rownames has_rows
#'   is_scalar
#' @importFrom assertive.sets assert_are_disjoint_sets
#'   assert_are_intersecting_sets assert_are_set_equal assert_is_subset
#'   is_subset
#' @importFrom assertive.strings assert_all_are_matching_regex
#'   assert_all_are_non_empty_character
#'   assert_all_are_non_missing_nor_empty_character
#'   assert_any_are_matching_regex
#' @importFrom assertive.types assert_is_a_bool assert_is_a_number
#'   assert_is_a_string assert_is_all_of assert_is_an_integer assert_is_any_of
#'   assert_is_call assert_is_character assert_is_data.frame
#'   assert_is_environment assert_is_factor assert_is_function assert_is_integer
#'   assert_is_list assert_is_logical assert_is_matrix assert_is_name
#'   assert_is_numeric assert_is_symbol assert_is_tbl_df is_a_number is_a_string
#' @importFrom assertthat assert_that validate_that
#' @importFrom cowplot plot_grid
#' @importFrom curl has_internet
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
#' @importFrom goalie assertAreNonExisting assertAreUniqueGeneNames
#'   assertAreURLs assertAreValidNames assertFormalCompress assertHasRownames
#'   assertHasValidDimnames assertIsAlpha assertIsANumberOrNULL
#'   assertIsAnImplicitInteger assertIsAnImplicitIntegerOrNULL
#'   assertIsAnIntegerOrNULL assertIsColorScaleContinuousOrNULL
#'   assertIsColorScaleDiscreteOrNULL assertIsDir assertIsFile
#'   assertIsFillScaleContinuousOrNULL assertIsFillScaleDiscreteOrNULL
#'   assertIsHeaderLevel assertIsHexColorFunctionOrNULL assertIsImplicitInteger
#'   assertIsStringOrNULL hasRownames hasUniqueCols isAnImplicitInteger isURL
#' @importFrom grDevices colorRampPalette
#' @importFrom grid arrow unit
#' @importFrom jsonlite read_json
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom magrittr %>% set_colnames set_rownames
#' @importFrom matrixStats rowVars
#' @importFrom methods as formalArgs getGeneric getMethod is isGeneric new
#'   selectMethod setAs setClass setGeneric setMethod setOldClass setValidity
#'   show signature slot slotNames validObject .hasSlot
#' @importFrom pbapply pblapply
#' @importFrom pheatmap pheatmap
#' @importFrom purrr map
#' @importFrom readr cols read_lines read_tsv write_csv write_lines
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#' @importFrom rlang !! !!! := dots_list eval_bare has_length sym syms UQ
#' @importFrom rtracklayer import
#' @importFrom sessioninfo session_info
#' @importFrom stats as.formula prcomp quantile
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

# FIXME R is caching this in heatmap methods and I can't figure out why...
#' @importFrom goalie areSamplesUnique
NULL

# Conflicts with rtracklayer:
# @importFrom rio import

# Conflicts with BiocGenerics:
# @importMethodsFrom Matrix colSums rowMeans rowSums t
