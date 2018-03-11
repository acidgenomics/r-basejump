#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @import methods
#' @importFrom assertive assert_all_are_dirs
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
#' @importFrom assertive assert_is_scalar
#' @importFrom assertive assert_is_subset
#' @importFrom assertive assert_is_vector
#' @importFrom assertive has_colnames
#' @importFrom assertive has_rows
#' @importFrom assertive is_a_number
#' @importFrom assertive is_a_string
#' @importFrom assertive is_existing
#' @importFrom assertive is_hex_color
#' @importFrom assertive is_scalar
#' @importFrom rlang abort inform warn
#' @importFrom S4Vectors mcols mcols<- na.omit tail
#' @importFrom utils capture.output globalVariables
"_PACKAGE"



globalVariables(".")
ensemblReturn <- c(
    "GRanges",
    "DataFrame",
    "data.frame"
)
annotationCols <- c(
    "txID",
    "txName",
    "txBiotype",
    "geneID",
    "geneName",
    "geneBiotype",
    "broadClass",
    "description"
)
