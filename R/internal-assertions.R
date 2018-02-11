# nolint begin

#' @seealso base::save
#' @noRd
.assert_compress <- function(x) {
    assert_is_any_of(x, c("character", "logical"))
    if (is.character(x)) {
        assert_is_a_string(x)
        assert_is_subset(x, c("bzip2", "gzip", "xz"))
    }
}



.assert_markdown_header_level <- function(x) {
    assert_is_numeric(x)
    assert_is_scalar(x)
    assert_is_subset(as.integer(x), seq(1L:7L))
}



.assert_is_a_string_or_null <- function(x) {
    assert_is_any_of(x, c("character", "NULL"))
    if (is.character(x)) {
        assert_is_a_string(x)
    }
}



.assert_is_numeric_scalar_or_null <- function(x) {
    assert_is_any_of(x, c("numeric", "NULL"))
    if (is.numeric(x)) {
        assert_is_scalar(x)
    }
}

# nolint end
