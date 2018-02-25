#' Assert Is an Integer or NULL
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @export
#'
#' @examples
#' # Success
#' assertIsAnIntegerOrNULL(1L)
#' assertIsAnIntegerOrNULL(NULL)
#'
#' # Failure
#' tryCatch(
#'     assertIsAnIntegerOrNULL(c(1L, 2L)),
#'     error = function(e) e)
assertIsAnIntegerOrNULL <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("integer", "NULL"),
        severity = severity)
    if (is.integer(x)) {
        assert_is_an_integer(x, severity = severity)
    }
}
