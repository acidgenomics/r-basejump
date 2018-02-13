#' Compression Formal Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
assert_formal_compress <- function(x) {  # nolint
    assert_is_any_of(x, c("character", "logical"))
    if (is.character(x)) {
        assert_is_a_string(x)
        assert_is_subset(x, c("bzip2", "gzip", "xz"))
    }
}
