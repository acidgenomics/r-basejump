#' Is a String or NULL
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
assert_is_a_string_or_null <- function(x) {  # nolint
    assert_is_character_or_null(x)
    if (is.character(x)) {
        assert_is_a_string(x)
    }
}
