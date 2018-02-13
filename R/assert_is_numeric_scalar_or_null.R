#' Is Numeric Scalar or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
assert_is_numeric_scalar_or_null <- function(x) {
    assert_is_any_of(x, c("numeric", "NULL"))
    if (is.numeric(x)) {
        assert_is_scalar(x)
    }
}
