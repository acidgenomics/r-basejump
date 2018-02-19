#' Assert Checks
#'
#' @rdname assert
#' @name assert
#' @family Assert Checks
#' @keywords internal
#'
#' @importFrom assertive assert_all_are_existing_files
#' @importFrom assertive assert_all_are_greater_than
#' @importFrom assertive assert_all_are_greater_than_or_equal_to
#' @importFrom assertive assert_all_are_hex_colors
#' @importFrom assertive assert_all_are_matching_regex
#' @importFrom assertive assert_all_are_non_missing_nor_empty_character
#' @importFrom assertive assert_all_are_non_negative
#' @importFrom assertive assert_all_are_not_na
#' @importFrom assertive assert_all_are_positive
#' @importFrom assertive assert_any_are_matching_regex
#' @importFrom assertive assert_are_disjoint_sets
#' @importFrom assertive assert_are_intersecting_sets
#' @importFrom assertive assert_are_identical
#' @importFrom assertive assert_are_same_length
#' @importFrom assertive assert_has_dimnames
#' @importFrom assertive assert_has_colnames
#' @importFrom assertive assert_has_dims
#' @importFrom assertive assert_has_names
#' @importFrom assertive assert_has_no_duplicates
#' @importFrom assertive assert_is_a_bool
#' @importFrom assertive assert_is_a_number
#' @importFrom assertive assert_is_a_string
#' @importFrom assertive assert_is_all_of
#' @importFrom assertive assert_is_an_integer
#' @importFrom assertive assert_is_any_of
#' @importFrom assertive assert_is_atomic
#' @importFrom assertive assert_is_character
#' @importFrom assertive assert_is_data.frame
#' @importFrom assertive assert_is_empty
#' @importFrom assertive assert_is_environment
#' @importFrom assertive assert_is_factor
#' @importFrom assertive assert_is_identical_to_na
#' @importFrom assertive assert_is_integer
#' @importFrom assertive assert_is_list
#' @importFrom assertive assert_is_matrix
#' @importFrom assertive assert_is_name
#' @importFrom assertive assert_is_non_empty
#' @importFrom assertive assert_is_not_null
#' @importFrom assertive assert_is_numeric
#' @importFrom assertive assert_is_scalar
#' @importFrom assertive assert_is_subset
#' @importFrom assertive assert_is_vector
#' @importFrom assertive has_colnames
#' @importFrom assertive is_a_string
#' @importFrom assertive is_existing
#' @importFrom assertive is_hex_color
#' @importFrom assertive is_scalar
#'
#' @param x Object.
#' @param envir Environment.
#' @param inherits Should the enclosing frames of the environment be searched?
#' @param severity How severe should the consequences of the assertion be?
#'   Either "`stop`", "`warning`", "`message`", or "`none`".
#'
#' @return Abort on mismatch.
NULL



# TODO Use assert engine and add severity
#' All Variables Do Not Exist Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @param x Character vector of variable names to check in environment.
#'
#' @export
#'
#' @examples
#' a <- 1L
#' b <- 2L
#' assert_all_are_non_existing(c("a", "b", "c"))
assert_all_are_non_existing <- function(  # nolint
    x,
    envir = parent.frame(),
    inherits = FALSE) {
    exists <- is_existing(x, envir = envir, inherits = inherits)
    if (any(exists)) {
        abort(paste(
            "Already exists in environment:",
            toString(x[exists])
        ))
    }
}



#' Annotation Column Formal Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @param colData Column data.
#'
#' @export
assert_formal_annotation_col <- function(  # nolint
    x,
    colData,
    severity = "stop") {
    assert_has_dims(x, severity = severity)
    assert_is_any_of(
        x = colData,
        classes = c("data.frame", "logical", "NULL"),
        severity = severity)
    if (is.data.frame(colData)) {
        assert_has_colnames(colData, severity = severity)
        assert_has_rownames(colData, severity = severity)
        assert_are_identical(
            x = colnames(x),
            y = rownames(colData),
            severity = severity)
        lapply(colData, assert_is_factor)
    }
    if (is.logical(colData)) {
        assert_is_identical_to_na(colData, severity = severity)
    }
}



#' Color Function Formal Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @export
assert_formal_color_function <- function(  # nolint
    x,
    severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("function", "NULL"),
        severity = severity)
    if (is.function(x)) {
        colors <- x(2L)
        assert_is_character(colors, severity = severity)
        if (!all(is_hex_color(colors))) {
            # viridis adds "FF" to the end of hex colors.
            # Attempt to fix before running hex check.
            colors <- gsub("^(#[A-Z0-9]{6})[A-Z0-9]{2}$", "\\1", colors)
        }
        assert_all_are_hex_colors(colors, severity = severity)
    }
}



#' Compression Formal Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @export
assert_formal_compress <- function(  # nolint
    x,
    severity = "stop") {
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



#' Gene to Symbol Mappings Formal Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @param x Object containing gene identifiers in the rownames.
#' @param genes Character vector of gene identifiers.
#' @param gene2symbol Gene-to-symbol mappings data frame.
#'
#' @export
assert_formal_gene2symbol <- function(  # nolint
    x,
    genes,
    gene2symbol,
    severity = "stop") {
    assert_is_any_of(genes, c("character", "NULL"))
    if (is.character(genes)) {
        assert_is_subset(genes, rownames(x))
    }
    assert_is_any_of(gene2symbol, c("data.frame", "NULL"))
    if (is.data.frame(gene2symbol)) {
        assert_is_gene2symbol(gene2symbol)
        assert_is_subset(rownames(x), rownames(gene2symbol))
    }
}



#' Markdown Header Level Formal Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @export
assert_formal_header_level <- function(  # nolint
    x,
    severity = "stop") {
    assert_is_numeric(x, severity = severity)
    assert_is_scalar(x, severity = severity)
    assert_is_subset(
        x = as.integer(x),
        y = seq(1L:7L),
        severity = severity)
}



# TODO Use assert engine
#' Has Rownames Assert Check
#'
#' A stricter alternative to the assertive version that works properply with
#' data frames.
#'
#' @note `tibble::has_rownames()` works better than `assertive::has_rownames()`
#'   for data frames and tibbles.
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @export
assert_has_rownames <- function(  # nolint
    x,
    severity = "stop") {
    stopifnot(has_rownames(x))
    assert_are_disjoint_sets(
        x = rownames(x),
        y = as.character(seq_len(nrow(x))),
        severity = severity
    )
}



#' Is a Number or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @export
assert_is_a_number_or_null <- function(  # nolint
    x,
    severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("numeric", "NULL"),
        severity = severity)
    if (is.numeric(x)) {
        assert_is_a_number(x, severity = severity)
    }
}



#' Is a String or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @export
assert_is_a_string_or_null <- function(  # nolint
    x,
    severity = "stop") {
    assert_is_character_or_null(x, severity = severity)
    if (is.character(x)) {
        assert_is_a_string(x, severity = severity)
    }
}



#' @rdname assert_is_implicit_integer
#' @export
assert_is_an_implicit_integer <- function(  # nolint
    x) {
    assert_is_implicit_integer(x)
    assert_is_scalar(x)
}



#' @rdname assert_is_implicit_integer
#' @export
assert_is_an_implicit_integer_or_null <- function(
    x) {
    assert_is_implicit_integer_or_null(x)
    if (is_implicit_integer(x)) {
        assert_is_a_number(x)
    }
}



#' Is an Integer or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @export
assert_is_an_integer_or_null <- function(  # nolint
    x,
    severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("character", "NULL"),
        severity = severity)
    if (is.character(x)) {
        assert_is_an_integer(x, severity = severity)
    }
}



#' Ensembl Annotations Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @param x [data.frame] containing Ensembl gene annotations returned
#'   from the [annotable()] function.
#'
#' @export
#'
#' @examples
#' annotable <- annotable("Homo sapiens")
#' assert_is_annotable(annotable)
assert_is_annotable <- function(  # nolint
    x,
    severity = "stop") {
    assert_is_data.frame(x, severity = severity)
    assert_is_subset(
        x = c("ensgene", "symbol", "description", "biotype", "broadClass"),
        y = colnames(x),
        severity = severity)
}



#' Character Vector or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @export
assert_is_character_or_null <- function(  # nolint
    x,
    severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("character", "NULL"),
        severity = severity)
}



#' Data Frame or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @export
assert_is_data.frame_or_null <- function(  # nolint
    x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("data.frame", "NULL"),
        severity = severity)
}



#' Check Gene to Symbol Mapping Data
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @param x [data.frame] containing Ensembl gene identifier to gene symbol
#'   mappings. Must be structured as a two column [data.frame] with "ensgene"
#'   and "symbol" columns.
#'
#' @export
#'
#' @examples
#' gene2symbol <- annotable("Homo sapiens", format = "gene2symbol")
#' assert_is_gene2symbol(gene2symbol)
assert_is_gene2symbol <- function(  # nolint
    x,
    severity = "stop") {
    assert_is_data.frame(x, severity = severity)
    assert_are_identical(
        x = colnames(x),
        y = c("ensgene", "symbol"),
        severity = severity)
}



# TODO Use assert engine
#' Implicit Integer Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @export
assert_is_implicit_integer <- function(  # nolint
    x) {
    stopifnot(is_implicit_integer(x))
}



# TODO Use assert engine
#' @rdname assert_is_implicit_integer
#' @export
assert_is_implicit_integer_or_null <- function(  # nolint
    x) {
    stopifnot(any(is_implicit_integer(x), is.null(x)))
}



#' Check Transcript to Gene Mapping Data
#'
#' @family Assert Checks
#' @inherit assert
#' @keywords internal
#'
#' @param x [data.frame] containing Ensembl transcript to gene identifier
#'   mappings. Must be structured as a two column [data.frame] with "enstxp" and
#'   "ensgene" columns.
#'
#' @export
#'
#' @examples
#' tx2gene <- annotable("Homo sapiens", format = "tx2gene")
#' assert_is_tx2gene(tx2gene)
assert_is_tx2gene <- function(  # nolint
    x,
    severity = "stop") {
    assert_is_data.frame(x, severity = severity)
    assert_are_identical(
        x = colnames(x),
        y = c("enstxp", "ensgene"),
        severity = severity)
}



#' @rdname assert_has_rownames
#' @export
has_rownames <- function(x) {  # nolint
    if (is.data.frame(x)) {
        tibble::has_rownames(x)
    } else {
        assertive::has_rownames(x)
    }
}



# TODO Use assert engine
#' @rdname assert_is_implicit_integer
#' @export
is_implicit_integer <- function(  # nolint
    x) {
    if (!is.numeric(x)) {
        return(FALSE)
    }
    if (is.integer(x)) {
        return(TRUE)
    }
    isTRUE(all.equal(x, as.integer(x), tolerance = .Machine[["double.eps"]]))
}
