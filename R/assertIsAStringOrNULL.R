#' Is a String or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
assertIsAStringOrNULL <- function(x, severity = "stop") {
    assert_is_character_or_null(x, severity = severity)
    if (is.character(x)) {
        assert_is_a_string(x, severity = severity)
    }
}



# Aliases ======================================================================
#' @rdname assertIsAStringOrNULL
#' @export
assertIsAStringOrNULL -> assert_is_a_string_or_null
