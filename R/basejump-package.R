#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importFrom AnnotationDbi columns
#' @importFrom AnnotationHub AnnotationHub query snapshotDate
#' @importFrom assertive.base assert_all_are_not_na assert_are_identical
#'   assert_is_identical_to_na is_not_na
#' @importFrom assertive.code assert_all_are_existing is_existing
#' @importFrom assertive.data assert_all_are_hex_colors is_hex_color
#' @importFrom assertive.files assert_all_are_dirs assert_all_are_existing_files
#' @importFrom assertive.numbers assert_all_are_greater_than
#'   assert_all_are_greater_than_or_equal_to assert_all_are_non_negative
#'   assert_all_are_positive
#' @importFrom assertive.properties assert_are_same_length assert_has_colnames
#'   assert_has_dimnames assert_has_dims assert_has_names
#'   assert_has_no_duplicates assert_has_rows assert_is_atomic assert_is_empty
#'   assert_is_non_empty assert_is_not_null assert_is_of_length
#'   assert_is_scalar assert_is_vector has_colnames has_dims has_rows is_scalar
#' @importFrom assertive.sets assert_are_disjoint_sets
#'   assert_are_intersecting_sets assert_are_set_equal assert_is_subset
#' @importFrom assertive.strings assert_all_are_matching_regex
#'   assert_all_are_non_missing_nor_empty_character
#'   assert_any_are_matching_regex
#' @importFrom assertive.types assert_is_a_bool assert_is_a_number
#'   assert_is_a_string assert_is_all_of assert_is_an_integer assert_is_any_of
#'   assert_is_character assert_is_data.frame assert_is_environment
#'   assert_is_factor assert_is_integer assert_is_list assert_is_matrix
#'   assert_is_name assert_is_numeric is_a_number is_a_string
#' @importFrom cowplot plot_grid
#' @importFrom dplyr arrange case_when everything filter funs group_by mutate
#'   mutate_all mutate_at mutate_if select starts_with summarize_all top_n
#'   ungroup
#' @importFrom ensembldb ensemblVersion organism
#' @importFrom GenomicFeatures genes makeTxDbFromGFF transcripts
#' @importFrom GenomicRanges GRanges
#' @importFrom ggplot2 element_blank element_line element_rect element_text
#'   theme theme_linedraw
#' @importFrom IRanges IRanges
#' @importFrom jsonlite read_json
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom Matrix colSums rowMeans rowSums readMM t writeMM
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom magrittr set_colnames set_names
#' @importFrom methods as getGeneric is selectMethod show validObject
#' @importFrom pbapply pblapply
#' @importFrom R.utils gzip
#' @importFrom RCurl getURL
#' @importFrom readr read_lines read_tsv write_csv write_lines
#' @importFrom rlang !! UQ sym dots_list eval_bare
#' @importFrom rtracklayer import
#' @importFrom S4Vectors as.data.frame complete.cases mcols<- metadata
#'   metadata<- na.omit tail
#' @importFrom stats dist
#' @importFrom stringr regex str_dup str_extract str_length str_match str_pad
#'   str_replace str_replace_all str_replace_na str_subset
#' @importFrom SummarizedExperiment assays colData colData<-
#' @importFrom tibble as_tibble column_to_rownames has_rownames is_tibble
#'   rownames_to_column
#' @importFrom tidyr separate unite
#' @importFrom utils capture.output download.file globalVariables
#'   installed.packages packageVersion read.delim read.table
#' @importFrom yaml yaml.load_file
"_PACKAGE"

# NAMESPACE conflicts:
# @importFrom rio import
