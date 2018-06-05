#' Assert Checks
#'
#' @name assert
#' @keywords internal
#'
#' @param x Object.
#' @param envir `environment`.
#' @param inherits Should the enclosing frames of the `environment` be searched?
#' @param severity How severe should the consequences of the assertion be?
#'   Either "`stop`", "`warning`", "`message`", or "`none`".
#'
#' @return Stop on mismatch.
NULL



#' Assert Is Implicit Integer
#'
#' @name assertIsImplicitInteger
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
NULL



#' Assert All Variables Are Non-Existing
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @param x `character` vector of variable names to check in `environment`.
#'
#' @export
#'
#' @examples
#' assertAllAreNonExisting(c("XXX", "YYY"))
assertAllAreNonExisting <- function(
    x,
    envir = parent.frame(),
    inherits = FALSE
) {
    exists <- is_existing(x, envir = envir, inherits = inherits)
    if (any(exists)) {
        stop(paste(
            "Already exists in environment:",
            toString(x[exists])
        ))
    }
}



#' Assert Are Ensembl Gene Annotations
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl gene annotations.
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'     "geneID" = "ENSG00000000003",
#'     "geneName" = "TSPAN6"
#' )
#' assertAreGeneAnnotations(x)
assertAreGeneAnnotations <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    x <- as.data.frame(x)
    assert_is_subset(
        x = c("geneID", "geneName"),
        y = colnames(x),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}



#' Assert Are Ensembl Transcript Annotations
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl transcript annotations.
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'     "txID" = "ENST00000000233",
#'     "geneID" = "ENSG00000004059"
#' )
#' assertAreTranscriptAnnotations(x)
assertAreTranscriptAnnotations <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    x <- as.data.frame(x)
    assert_is_subset(
        x = c("txID", "geneID"),
        y = colnames(x),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}



#' Assert Formal Compression
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertFormalCompress("xz")
assertFormalCompress <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("character", "logical"),
        severity = severity
    )
    if (is.character(x)) {
        assert_is_a_string(x, severity = severity)
        assert_is_subset(
            x = x,
            y = c("bzip2", "gzip", "xz"),
            severity = severity
        )
    }
}



#' Assert Formal Gene to Symbol Mappings
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @param x Object containing gene identifiers in the row names.
#' @param genes `character` vector of gene identifiers.
#' @param gene2symbol Gene-to-symbol mappings. Must contain a `data.frame` or
#'   `NULL`.
#'
#' @export
#'
#' @examples
#' gene2symbol <- data.frame(
#'     "geneID" = c("ENSG00000000003", "ENSG00000000005"),
#'     "geneName" = c("TSPAN6", "TNMD"),
#'     row.names = c("ENSG00000000003", "ENSG00000000005")
#' )
#' genes <- head(rownames(gene2symbol), 2L)
#' x <- data.frame(
#'     "sample_1" = c(1L, 2L),
#'     "sample_2" = c(3L, 4L),
#'     row.names = genes,
#'     stringsAsFactors = FALSE
#' )
#' assertFormalGene2symbol(x, genes, gene2symbol)
assertFormalGene2symbol <- function(
    x,
    genes,
    gene2symbol,
    severity = getOption("assertive.severity", "stop")
) {
    assertHasRownames(x)
    assert_is_any_of(genes, c("character", "NULL"))
    if (is.character(genes)) {
        assert_is_subset(genes, rownames(x))
    }
    assert_is_any_of(gene2symbol, c("data.frame", "NULL"))
    if (is.data.frame(gene2symbol)) {
        assertIsGene2symbol(gene2symbol)
        assert_is_subset(rownames(x), rownames(gene2symbol))
    }
}



#' Assert Has Rownames
#'
#' A stricter alternative to the assertive version that works properply with
#' data frames.
#'
#' @note `tibble::has_rownames()` is more consistent than
#'   `assertive.properties::has_rownames()` for data frames and tibbles.
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'     "sample_1" = c(1L, 2L),
#'     "sample_2" = c(3L, 4L),
#'     row.names = c("gene_1", "gene_2"),
#'     stringsAsFactors = FALSE
#' )
#' assertHasRownames(data)
assertHasRownames <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    stopifnot(hasRownames(x))
    assert_are_disjoint_sets(
        x = rownames(x),
        y = as.character(seq_len(nrow(x))),
        severity = severity
    )
}



#' Assert Is a Header Level
#'
#' Markdown supports header levels 1-7 (`<H1>`-`<H7>`).
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsAHeaderLevel(1L)
assertIsAHeaderLevel <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_a_number(x, severity = severity)
    assert_is_subset(
        x = as.integer(x),
        y = seq(1L:7L),
        severity = severity
    )
}



#' Assert Is an Integer or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsAnIntegerOrNULL(1L)
#' assertIsAnIntegerOrNULL(NULL)
assertIsAnIntegerOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("integer", "NULL"),
        severity = severity
    )
    if (is.integer(x)) {
        assert_is_an_integer(x, severity = severity)
    }
}



#' Assert Is a Number or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsANumberOrNULL(1.1)
#' assertIsANumberOrNULL(NULL)
assertIsANumberOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("numeric", "NULL"),
        severity = severity
    )
    if (is.numeric(x)) {
        assert_is_a_number(x, severity = severity)
    }
}



#' Assert Is a String or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsAStringOrNULL("hello world")
#' assertIsAStringOrNULL(NULL)
assertIsAStringOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assertIsCharacterOrNULL(x, severity = severity)
    if (is.character(x)) {
        assert_is_a_string(x, severity = severity)
    }
}



# Deprecate this function in a future release
#' Assert Is Character Vector or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsCharacterOrNULL(c("hello", "world"))
#' assertIsCharacterOrNULL(NULL)
assertIsCharacterOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("character", "NULL"),
        severity = severity
    )
}



#' Assert Is Color Palette Scale Continuous or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' color <- ggplot2::scale_color_gradient(low = "red", high = "blue")
#' class(color)
#' assertIsColorScaleContinuousOrNULL(color)
#' assertIsColorScaleContinuousOrNULL(NULL)
assertIsColorScaleContinuousOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("ScaleContinuous", "NULL"),
        severity = severity
    )
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleContinuous"),
            severity = severity
        )
        assert_are_identical(
            x = x[["aesthetics"]],
            y = "colour",
            severity = severity
        )
    }
}



#' Assert Is Color Palette Scale Discrete or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' color <- ggplot2::scale_color_manual(values = c("red", "blue"))
#' class(color)
#' assertIsColorScaleDiscreteOrNULL(color)
#' assertIsColorScaleDiscreteOrNULL(NULL)
assertIsColorScaleDiscreteOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("ScaleDiscrete", "NULL"),
        severity = severity
    )
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleDiscrete"),
            severity = severity
        )
        assert_are_identical(
            x = x[["aesthetics"]],
            y = "colour",
            severity = severity
        )
    }
}



# Deprecate this function in a future release
#' Assert Is Data Frame or NULL
#'
#' @note This checks for `data.frame` and will stop on `DataFrame` class.
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsDataFrameOrNULL(datasets::mtcars)
#' assertIsDataFrameOrNULL(NULL)
assertIsDataFrameOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("data.frame", "NULL"),
        severity = severity
    )
}



#' Assert Is Fill Palette Scale Continuous or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' fill <- ggplot2::scale_fill_gradient(low = "red", high = "blue")
#' class(fill)
#' assertIsFillScaleContinuousOrNULL(fill)
#' assertIsFillScaleContinuousOrNULL(NULL)
assertIsFillScaleContinuousOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("ScaleContinuous", "NULL"),
        severity = severity
    )
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleContinuous"),
            severity = severity
        )
        assert_are_identical(
            x = x[["aesthetics"]],
            y = "fill",
            severity = severity
        )
    }
}



#' Assert Is Fill Palette Scale Discrete or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' fill <- ggplot2::scale_fill_manual(values = c("red", "blue"))
#' class(fill)
#' assertIsFillScaleDiscreteOrNULL(fill)
#' assertIsFillScaleDiscreteOrNULL(NULL)
assertIsFillScaleDiscreteOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("ScaleDiscrete", "NULL"),
        severity = severity
    )
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleDiscrete"),
            severity = severity
        )
        assert_are_identical(
            x = x[["aesthetics"]],
            y = "fill",
            severity = severity
        )
    }
}



#' Assert Is Gene to Symbol Mapping Data Frame
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl gene-to-symbol mappings. Must be
#'   structured as a two column `data.frame` with "`geneID`" and "`geneName`"
#'   columns.
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'     "geneID" = "ENSG00000000003",
#'     "geneName" = "TSPAN6"
#' )
#' assertIsGene2symbol(x)
assertIsGene2symbol <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_data.frame(x, severity = severity)
    assert_are_identical(
        x = colnames(x),
        y = c("geneID", "geneName"),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}



#' Assert Is Hex Color Function or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' hex <- function(n) {
#'     colors <- c("#FFFFFF", "#000000")
#'     colors[seq_len(n)]
#' }
#' assertIsHexColorFunctionOrNULL(hex)
#' assertIsHexColorFunctionOrNULL(NULL)
assertIsHexColorFunctionOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("function", "NULL"),
        severity = severity
    )
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



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsAnImplicitInteger(1)
assertIsAnImplicitInteger <- function(x) {
    stopifnot(isAnImplicitInteger(x))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsAnImplicitIntegerOrNULL(1)
#' assertIsAnImplicitIntegerOrNULL(NULL)
assertIsAnImplicitIntegerOrNULL <- function(x) {
    stopifnot(any(isAnImplicitInteger(x), is.null(x)))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsImplicitInteger(c(1, 2))
assertIsImplicitInteger <- function(x) {
    stopifnot(isImplicitInteger(x))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsImplicitIntegerOrNULL(c(1, 2))
#' assertIsImplicitIntegerOrNULL(NULL)
assertIsImplicitIntegerOrNULL <- function(x) {
    stopifnot(any(isImplicitInteger(x), is.null(x)))
}



#' Assert Is Transcript-to-Gene Mapping Data
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl transcript to gene identifier
#'   mappings. Must be structured as a two column `data.frame` with "txID" and
#'   "geneID" columns.
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'     "txID" = "ENST00000000233",
#'     "geneID" = "ENSG00000004059"
#' )
#' assertIsTx2gene(x)
assertIsTx2gene <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_data.frame(x, severity = severity)
    assert_are_identical(
        x = colnames(x),
        y = c("txID", "geneID"),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}



#' Assert Is URL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsURL("https://steinbaugh.com")
assertIsURL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_character(x, severity = severity)
    assert_all_are_matching_regex(
        x = x,
        pattern = "^http(s)?\\://.+",
        severity = severity
    )
}



# Logical Return Functions =====================================================
#' @rdname assertHasRownames
#' @export
hasRownames <- function(x) {  # nolint
    if (is.data.frame(x)) {
        tibble::has_rownames(x)
    } else {
        assertive.properties::has_rownames(x)
    }
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' isAnImplicitInteger(1)
isAnImplicitInteger <- function(x) {
    if (!is_a_number(x)) {
        return(FALSE)
    }
    isImplicitInteger(x)
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' isImplicitInteger(list(1, 1L, 1.1, "XXX"))
isImplicitInteger <- function(x) {
    mapply(
        x = x,
        FUN = function(x) {
            if (!is.numeric(x)) {
                return(FALSE)
            }
            if (is.integer(x)) {
                return(TRUE)
            }
            isTRUE(all.equal(
                target = as.integer(x),
                current = x,
                tolerance = .Machine[["double.eps"]]
            ))
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
}



#' @rdname assertIsURL
#' @export
isURL <- function(x) {
    if (!is.character(x)) {
        return(FALSE)
    }
    vapply(
        X = x,
        FUN = function(x) {
            grepl("^http(s)?\\://.+", x)
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
}
