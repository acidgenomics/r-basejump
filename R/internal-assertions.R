# nolint begin

#' @importFrom assertive assert_has_colnames
#' @importFrom assertive assert_has_rownames
#' @importFrom assertive assert_is_any_of
#' @importFrom assertive assert_is_identical_to_na
.assert_formal_annotation_col <- function(x) {
    assert_is_any_of(x, c("data.frame", "logical"))
    if (is.data.frame(x)) {
        assert_has_colnames(x)
        assert_has_rownames(x)
    }
    if (is.logical(x)) {
        assert_is_identical_to_na(x)
    }
}



#' @importFrom assertive assert_is_a_string
#' @importFrom assertive assert_is_any_of
#' @importFrom assertive assert_is_subset
#' @seealso base::save
#' @noRd
.assert_formal_compress <- function(x) {
    assert_is_any_of(x, c("character", "logical"))
    if (is.character(x)) {
        assert_is_a_string(x)
        assert_is_subset(x, c("bzip2", "gzip", "xz"))
    }
}



#' @importFrom assertive assert_is_numeric
#' @importFrom assertive assert_is_scalar
#' @importFrom assertive assert_is_subset
.assert_formal_header_level <- function(x) {
    assert_is_numeric(x)
    assert_is_scalar(x)
    assert_is_subset(as.integer(x), seq(1L:7L))
}



#' @importFrom assertive assert_is_a_string
#' @importFrom assertive assert_is_any_of
.assert_is_a_string_or_null <- function(x) {
    assert_is_any_of(x, c("character", "NULL"))
    if (is.character(x)) {
        assert_is_a_string(x)
    }
}



#' @importFrom assertive assert_is_any_of
#' @importFrom assertive assert_is_scalar
.assert_is_numeric_scalar_or_null <- function(x) {
    assert_is_any_of(x, c("numeric", "NULL"))
    if (is.numeric(x)) {
        assert_is_scalar(x)
    }
}

# nolint end
