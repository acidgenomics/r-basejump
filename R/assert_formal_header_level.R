#' Markdown Header Level Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
assert_formal_header_level <- function(x) {
    assert_is_numeric(x)
    assert_is_scalar(x)
    assert_is_subset(as.integer(x), seq(1L:7L))
}
