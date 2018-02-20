#' Assert Is a String or NULL
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
