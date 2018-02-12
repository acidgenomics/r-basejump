#' Assert Check Functions
#'
#' These functions are generally structured following the syntax in the
#' assertive package.
#'
#' @rdname assert
#' @name assert
#'
#' @inheritParams general
#' @keywords internal
#'
#' @importFrom assertive assert_all_are_existing_files
#' @importFrom assertive assert_all_are_greater_than_or_equal_to
#' @importFrom assertive assert_all_are_hex_colors
#' @importFrom assertive assert_all_are_matching_regex
#' @importFrom assertive assert_all_are_non_missing_nor_empty_character
#' @importFrom assertive assert_all_are_non_negative
#' @importFrom assertive assert_all_are_not_na
#' @importFrom assertive assert_all_are_positive
#' @importFrom assertive assert_any_are_matching_regex
#' @importFrom assertive assert_are_disjoint_sets
#' @importFrom assertive assert_are_identical
#' @importFrom assertive assert_are_same_length
#' @importFrom assertive assert_has_dimnames
#' @importFrom assertive assert_has_colnames
#' @importFrom assertive assert_has_dims
#' @importFrom assertive assert_has_names
#' @importFrom assertive assert_has_no_duplicates
#' @importFrom assertive assert_is_a_bool
#' @importFrom assertive assert_is_a_string
#' @importFrom assertive assert_is_any_of
#' @importFrom assertive assert_is_character
#' @importFrom assertive assert_is_data.frame
#' @importFrom assertive assert_is_environment
#' @importFrom assertive assert_is_factor
#' @importFrom assertive assert_is_identical_to_na
#' @importFrom assertive assert_is_integer
#' @importFrom assertive assert_is_list
#' @importFrom assertive assert_is_matrix
#' @importFrom assertive assert_is_name
#' @importFrom assertive assert_is_non_empty
#' @importFrom assertive assert_is_not_null
#' @importFrom assertive assert_is_scalar
#' @importFrom assertive assert_is_subset
#' @importFrom assertive assert_is_vector
#' @importFrom assertive has_colnames
#' @importFrom assertive is_a_string
#' @importFrom assertive is_hex_color
#' @importFrom assertive is_scalar
NULL



# nolint begin

#' @rdname assert
#' @param colData Column data.
#' @export
assert_formal_annotation_col <- function(object, colData) {
    assert_has_colnames(object)
    assert_is_any_of(colData, c("data.frame", "logical"))
    if (is.data.frame(colData)) {
        assert_has_colnames(colData)
        assert_has_rownames(colData)
        assert_are_identical(colnames(object), rownames(colData))
        # TODO Ensure that all columns are factors?
    }
    if (is.logical(colData)) {
        assert_is_identical_to_na(colData)
    }
}



#' @rdname assert
#' @export
assert_formal_color_function <- function(x) {
    assert_is_any_of(x, c("function", "NULL"))
    if (is.function(x)) {
        colors <- x(2L)
        assert_is_character(colors)
        if (!all(is_hex_color(colors))) {
            # viridis adds "FF" to the end of hex colors.
            # Attempt to fix before running hex check.
            colors <- gsub("^(#[A-Z0-9]{6})[A-Z0-9]{2}$", "\\1", colors)
        }
        assert_all_are_hex_colors(colors)
    }
}



#' @rdname assert
#' @export
assert_formal_compress <- function(x) {
    assert_is_any_of(x, c("character", "logical"))
    if (is.character(x)) {
        assert_is_a_string(x)
        assert_is_subset(x, c("bzip2", "gzip", "xz"))
    }
}



#' @rdname assert
#' @export
assert_formal_header_level <- function(x) {
    assert_is_numeric(x)
    assert_is_scalar(x)
    assert_is_subset(as.integer(x), seq(1L:7L))
}



#' @rdname assert
#' @note [assert_has_rownames()] is a stricter alternative to the assertive
#'   version that works properply with data frames.
#' @export
assert_has_rownames <- function(x) {
    stopifnot(has_rownames(x))
    assert_are_disjoint_sets(
        rownames(x),
        as.character(seq_len(nrow(x)))
    )
}



#' @rdname assert
#' @export
assert_is_a_string_or_null <- function(x) {
    assert_is_any_of(x, c("character", "NULL"))
    if (is.character(x)) {
        assert_is_a_string(x)
    }
}



#' @rdname assert
#' @export
assert_is_numeric_scalar_or_null <- function(x) {
    assert_is_any_of(x, c("numeric", "NULL"))
    if (is.numeric(x)) {
        assert_is_scalar(x)
    }
}



#' @rdname assert
#' @note `tibble::has_rownames()` works better than `assertive::has_rownames()`
#'   for data frames and tibbles.
#' @export
has_rownames <- function(x) {
    if (is.data.frame(x)) {
        tibble::has_rownames(x)
    } else {
        assertive::has_rownames(x)
    }
}

# nolint end
