#' Assert Is an Integer or NULL
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsAnIntegerOrNULL(1L)
#' assertIsAnIntegerOrNULL(NULL)
assertIsAnIntegerOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("integer", "NULL"),
        severity = severity
    )
    if (is.integer(x)) {
        assert_is_an_integer(x, severity = severity)
    }
}
