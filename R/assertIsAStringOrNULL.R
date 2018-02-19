#' Is a String or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
#'
#' @examples
#' # Success
#' assertIsAStringOrNULL("hello world")
#' assertIsAStringOrNULL(NULL)
#'
#' # Failure
#' tryCatch(
#'     assertIsAStringOrNULL(c("hello", "world")),
#'     error = function(e) e)
assertIsAStringOrNULL <- function(x, severity = "stop") {
    assertIsCharacterOrNULL(x, severity = severity)
    if (is.character(x)) {
        assert_is_a_string(x, severity = severity)
    }
}



# Aliases ======================================================================
#' @rdname assertIsAStringOrNULL
#' @export
assertIsAStringOrNULL -> assert_is_a_string_or_null
