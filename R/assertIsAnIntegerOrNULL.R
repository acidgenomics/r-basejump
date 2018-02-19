#' Is an Integer or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
#'
#' @examples
#' # Success
#' assertIsAnIntegerOrNULL(1L)
#'
#' # Failure (not scalar)
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



# Aliases ======================================================================
#' @rdname assertIsAnIntegerOrNULL
#' @export
assertIsAnIntegerOrNULL -> assert_is_an_integer_or_null
