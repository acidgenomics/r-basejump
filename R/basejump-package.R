#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @importFrom assertive assert_all_are_existing_files assert_are_identical
#'   assert_has_dimnames assert_is_a_bool assert_is_a_string assert_is_any_of
#'   assert_is_data.frame assert_is_environment assert_is_not_null
#'   assert_is_subset
#' @import methods
#' @importFrom rlang !! !!! .data abort dots_list eval_bare inform sym syms warn
#' @importFrom utils globalVariables
"_PACKAGE"

globalVariables(".")
