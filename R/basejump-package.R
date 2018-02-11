#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @import methods
#'
#' @importFrom assertive assert_all_are_existing_files
#' @importFrom assertive assert_all_are_hex_colors
#' @importFrom assertive assert_all_are_matching_regex
#' @importFrom assertive assert_all_are_non_empty_character
#' @importFrom assertive assert_all_are_not_na
#' @importFrom assertive assert_any_are_matching_regex
#' @importFrom assertive assert_are_identical
#' @importFrom assertive assert_has_colnames
#' @importFrom assertive assert_has_dimnames
#' @importFrom assertive assert_has_dims
#' @importFrom assertive assert_has_names
#' @importFrom assertive assert_has_no_duplicates
#' @importFrom assertive assert_has_rownames
#' @importFrom assertive assert_is_a_bool
#' @importFrom assertive assert_is_a_string
#' @importFrom assertive assert_is_any_of
#' @importFrom assertive assert_is_character
#' @importFrom assertive assert_is_data.frame
#' @importFrom assertive assert_is_environment
#' @importFrom assertive assert_is_factor
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
#' @importFrom assertive is_a_string
#' @importFrom assertive is_scalar
#'
#' @importFrom rlang abort inform warn
"_PACKAGE"



#' @importFrom utils globalVariables
globalVariables(".")
annotableCols <- c("ensgene", "symbol", "description", "biotype")
