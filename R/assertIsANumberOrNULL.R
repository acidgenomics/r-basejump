#' Assert Is a Number or NULL
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsANumberOrNULL(1.1)
#' assertIsANumberOrNULL(NULL)
assertIsANumberOrNULL <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("numeric", "NULL"),
        severity = severity
    )
    if (is.numeric(x)) {
        assert_is_a_number(x, severity = severity)
    }
}
