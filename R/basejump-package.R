#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @importFrom AnnotationHub AnnotationHub query snapshotDate
#' @importFrom GenomicFeatures genes makeTxDbFromGFF transcripts
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom Matrix colSums rowMeans rowSums readMM t writeMM
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom R.utils gzip
#' @importFrom RCurl getURL
#' @importFrom S4Vectors as.data.frame complete.cases mcols<- metadata na.omit
#'   tail
#' @importFrom cowplot plot_grid
#' @importFrom dplyr arrange case_when everything filter funs group_by mutate
#'   mutate_all mutate_at mutate_if select summarize_all top_n ungroup
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
#' @importFrom stringr regex str_dup str_extract str_length str_match str_pad
#'   str_replace str_replace_all str_replace_na str_subset
#' @importFrom tibble as_tibble column_to_rownames has_rownames is_tibble
#'   rownames_to_column
#' @importFrom tidyr separate
#' @importFrom utils capture.output download.file globalVariables
#'   installed.packages packageVersion read.delim read.table
#'
#' @importFrom assertive.base assert_all_are_not_na
#' @importFrom assertive.base assert_are_identical
#' @importFrom assertive.base assert_is_identical_to_na
#' @importFrom assertive.base is_not_na
#' @importFrom assertive.code assert_all_are_existing
#' @importFrom assertive.code is_existing
#' @importFrom assertive.data assert_all_are_hex_colors
#' @importFrom assertive.data is_hex_color
#' @importFrom assertive.files assert_all_are_dirs
#' @importFrom assertive.files assert_all_are_existing_files
#' @importFrom assertive.numbers assert_all_are_greater_than
#' @importFrom assertive.numbers assert_all_are_greater_than_or_equal_to
#' @importFrom assertive.numbers assert_all_are_non_negative
#' @importFrom assertive.numbers assert_all_are_positive
#' @importFrom assertive.properties assert_are_same_length
#' @importFrom assertive.properties assert_has_colnames
#' @importFrom assertive.properties assert_has_dimnames
#' @importFrom assertive.properties assert_has_dims
#' @importFrom assertive.properties assert_has_names
#' @importFrom assertive.properties assert_has_no_duplicates
#' @importFrom assertive.properties assert_has_rows
#' @importFrom assertive.properties assert_is_atomic
#' @importFrom assertive.properties assert_is_empty
#' @importFrom assertive.properties assert_is_non_empty
#' @importFrom assertive.properties assert_is_not_null
#' @importFrom assertive.properties assert_is_of_length
#' @importFrom assertive.properties assert_is_scalar
#' @importFrom assertive.properties assert_is_vector
#' @importFrom assertive.properties has_colnames
#' @importFrom assertive.properties has_dims
#' @importFrom assertive.properties has_rows
#' @importFrom assertive.properties is_scalar
#' @importFrom assertive.sets assert_are_disjoint_sets
#' @importFrom assertive.sets assert_are_intersecting_sets
#' @importFrom assertive.sets assert_are_set_equal
#' @importFrom assertive.sets assert_is_subset
#' @importFrom assertive.strings assert_all_are_matching_regex
#' @importFrom assertive.strings assert_all_are_non_missing_nor_empty_character
#' @importFrom assertive.strings assert_any_are_matching_regex
#' @importFrom assertive.types assert_is_a_bool
#' @importFrom assertive.types assert_is_a_number
#' @importFrom assertive.types assert_is_a_string
#' @importFrom assertive.types assert_is_all_of
#' @importFrom assertive.types assert_is_an_integer
#' @importFrom assertive.types assert_is_any_of
#' @importFrom assertive.types assert_is_character
#' @importFrom assertive.types assert_is_data.frame
#' @importFrom assertive.types assert_is_environment
#' @importFrom assertive.types assert_is_factor
#' @importFrom assertive.types assert_is_integer
#' @importFrom assertive.types assert_is_list
#' @importFrom assertive.types assert_is_matrix
#' @importFrom assertive.types assert_is_name
#' @importFrom assertive.types assert_is_numeric
#' @importFrom assertive.types is_a_number
#' @importFrom assertive.types is_a_string
"_PACKAGE"
