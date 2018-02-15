# TODO Use assert engine

#' Is Numeric Scalar or NULL Assert Check
#'
#' @family Assert Checks
#'
#' @inherit assert
#' @inheritParams general
#'
#' @export
assert_is_a_number_or_null <- function(x) {
    assert_is_any_of(x, c("numeric", "NULL"))
    if (is.numeric(x)) {
        assert_is_a_number(x)
    }
}
