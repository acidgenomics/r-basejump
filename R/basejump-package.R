#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @keywords internal
#'
#' @importFrom AnnotationHub AnnotationHub query snapshotDate
#' @importFrom GenomicFeatures genes makeTxDbFromGFF transcripts
#' @importFrom Matrix colSums rowMeans rowSums readMM t writeMM
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom R.utils gzip
#' @importFrom RCurl getURL
#' @importFrom S4Vectors as.data.frame complete.cases mcols<- metadata na.omit
#'   tail
#' @importFrom cowplot plot_grid
#' @importFrom dplyr arrange bind_rows case_when everything filter funs mutate
#'   mutate_all mutate_at mutate_if select summarize_all top_n
#' @importFrom ensembldb ensemblVersion organism
#' @importFrom ggplot2 aes element_blank element_line element_rect element_text
#'   ggplot theme theme_classic theme_minimal
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom magrittr set_colnames set_names
#' @importFrom methods as getGeneric is selectMethod show
#' @importFrom pbapply pblapply
#' @importFrom plyr ldply
#' @importFrom readr read_csv read_lines read_tsv write_csv write_lines
#' @importFrom readxl read_excel
#' @importFrom rlang !! UQ sym dots_list eval_bare
#' @importFrom stats dist
#' @importFrom stringr str_dup str_extract str_length str_match str_pad
#'   str_replace str_replace_all str_replace_na str_subset
#' @importFrom tibble as_tibble column_to_rownames has_rownames is_tibble
#'   rownames_to_column
#' @importFrom tidyr separate
#' @importFrom utils capture.output download.file globalVariables
#'   installed.packages packageVersion read.delim read.table
#' @importFrom yaml yaml.load_file
#'
#' @importFrom assertive assert_all_are_dirs
#' @importFrom assertive assert_all_are_existing
#' @importFrom assertive assert_all_are_existing_files
#' @importFrom assertive assert_all_are_greater_than
#' @importFrom assertive assert_all_are_greater_than_or_equal_to
#' @importFrom assertive assert_all_are_hex_colors
#' @importFrom assertive assert_all_are_matching_regex
#' @importFrom assertive assert_all_are_non_missing_nor_empty_character
#' @importFrom assertive assert_all_are_non_negative
#' @importFrom assertive assert_all_are_not_na
#' @importFrom assertive assert_all_are_positive
#' @importFrom assertive assert_any_are_matching_regex
#' @importFrom assertive assert_are_disjoint_sets
#' @importFrom assertive assert_are_intersecting_sets
#' @importFrom assertive assert_are_identical
#' @importFrom assertive assert_are_same_length
#' @importFrom assertive assert_are_set_equal
#' @importFrom assertive assert_has_dimnames
#' @importFrom assertive assert_has_colnames
#' @importFrom assertive assert_has_dims
#' @importFrom assertive assert_has_dimnames
#' @importFrom assertive assert_has_names
#' @importFrom assertive assert_has_no_duplicates
#' @importFrom assertive assert_has_rows
#' @importFrom assertive assert_is_a_bool
#' @importFrom assertive assert_is_a_number
#' @importFrom assertive assert_is_a_string
#' @importFrom assertive assert_is_all_of
#' @importFrom assertive assert_is_an_integer
#' @importFrom assertive assert_is_any_of
#' @importFrom assertive assert_is_atomic
#' @importFrom assertive assert_is_character
#' @importFrom assertive assert_is_data.frame
#' @importFrom assertive assert_is_empty
#' @importFrom assertive assert_is_environment
#' @importFrom assertive assert_is_factor
#' @importFrom assertive assert_is_identical_to_na
#' @importFrom assertive assert_is_integer
#' @importFrom assertive assert_is_list
#' @importFrom assertive assert_is_matrix
#' @importFrom assertive assert_is_name
#' @importFrom assertive assert_is_non_empty
#' @importFrom assertive assert_is_not_null
#' @importFrom assertive assert_is_numeric
#' @importFrom assertive assert_is_of_length
#' @importFrom assertive assert_is_scalar
#' @importFrom assertive assert_is_subset
#' @importFrom assertive assert_is_vector
#' @importFrom assertive has_colnames
#' @importFrom assertive has_dims
#' @importFrom assertive has_rows
#' @importFrom assertive is_a_number
#' @importFrom assertive is_a_string
#' @importFrom assertive is_existing
#' @importFrom assertive is_hex_color
#' @importFrom assertive is_not_na
#' @importFrom assertive is_scalar
"_PACKAGE"
