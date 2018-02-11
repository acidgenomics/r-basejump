# nolint begin

#' @importFrom assertive assert_has_colnames
#' @importFrom assertive assert_has_rownames
#' @importFrom assertive assert_is_any_of
#' @importFrom assertive assert_is_identical_to_na
.assert_formal_annotation_col <- function(x) {
    assert_is_any_of(x, c("data.frame", "logical"))
    if (is.data.frame(x)) {
        assert_has_colnames(x)
        .assert_has_rownames_strict(x)
    }
    if (is.logical(x)) {
        assert_is_identical_to_na(x)
    }
}



#' @importFrom assertive assert_is_any_of
#' @importFrom assertive assert_all_are_hex_colors
#' @importFrom assertive assert_is_character
#' @importFrom assertive is_hex_color
#' @noRd
.assert_formal_color_function <- function(x) {
    assert_is_any_of(x, c("function", "NULL"))
    if (is.function(x)) {
        colors <- x(2L)
        assert_is_character(colors)
        if (!all(is_hex_color(colors))) {
            # viridis adds "FF" to the end of hex colors.
            # Attempt to fix before running assertive check.
            colors <- gsub("^(#[A-Z0-9]{6})[A-Z0-9]{2}$", "\\1", colors)
        }
        assert_all_are_hex_colors(colors)
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



#' @importFrom assertive assert_are_disjoint_sets
#' @importFrom assertive assert_has_rownames
.assert_has_rownames_strict <- function(x) {
    assert_has_rownames(x)
    stopifnot(tibble::has_rownames(x))
    assert_are_disjoint_sets(
        rownames(x),
        as.character(seq_len(nrow(x)))
    )
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
