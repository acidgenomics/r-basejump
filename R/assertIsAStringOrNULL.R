#' Is a String or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
assertIsAStringOrNULL <- function(x, severity = "stop") {
    assertIsCharacterOrNull(x, severity = severity)
    if (is.character(x)) {
        assert_is_a_string(x, severity = severity)
    }
}



# Aliases ======================================================================
#' @rdname assertIsAStringOrNULL
#' @export
assertIsAStringOrNULL -> assert_is_a_string_or_null
