#' Is a Number or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
assertIsANumberOrNULL <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("numeric", "NULL"),
        severity = severity)
    if (is.numeric(x)) {
        assert_is_a_number(x, severity = severity)
    }
}



# Aliases ======================================================================
#' @rdname assertIsANumberOrNULL
#' @export
assertIsANumberOrNULL -> assert_is_a_number_or_null
