#' Compression Formal Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertFormalCompress("xz")
assertFormalCompress <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("character", "logical"),
        severity = severity)
    if (is.character(x)) {
        assert_is_a_string(x, severity = severity)
        assert_is_subset(
            x = x,
            y = c("bzip2", "gzip", "xz"),
            severity = severity)
    }
}



# Aliases ======================================================================
#' @rdname assertFormalCompress
#' @export
assertFormalCompress -> assert_formal_compress
