#' Assert Checks
#'
#' @name assert
#' @keywords internal
#'
#' @param x Object.
#' @param envir `environment`.
#' @param inherits `boolean`. Should the enclosing frames of the `environment`
#'   be searched?
#' @param severity `string`. How severe should the consequences of the assertion
#'   be? Either "`stop`", "`warning`", "`message`", or "`none`".
#'
#' @return Stop on mismatch.
NULL



# assertAllAreNonExisting ======================================================
#' Assert All Variables Are Non-Existing
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @param x `character` vector of variable names to check in `environment`.
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



# assertAllAreValidNames =======================================================
#' Assert All Are Valid Names
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @examples
#' assertAllAreValidNames(c("sample_1", "sample_2"))
assertAllAreValidNames <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_character(x, severity = severity)
    assert_is_non_empty(x, severity = severity)
    assert_all_are_true(validNames(x), severity = severity)
}



#' @rdname assertAllAreValidNames
#' @export
#' @examples
#' validNames(c(
#'     "sample_1",
#'     "gene_1",
#'     "293cells",
#'     "cell-AAAAAAAA",
#'     "GFP+ sort"
#' ))
validNames <- function(x) {
    if (!is.character(x) || length(x) == 0L) {
        return(FALSE)
    }
    vapply(
        X = x,
        FUN = function(x) {
            # Note that we're enforcing unique values here.
            identical(x, make.names(x, unique = TRUE))
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
}



# assertAreGeneAnnotations =====================================================
#' Assert Are Ensembl Gene Annotations
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @param x `data.frame` containing Ensembl gene annotations.
#'
#' @examples
#' x <- data.frame(
#'     geneID = "ENSG00000000003",
#'     geneName = "TSPAN6"
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



# assertAreTranscriptAnnotations ===============================================
#' Assert Are Ensembl Transcript Annotations
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @param x `data.frame` containing Ensembl transcript annotations.
#'
#'
#' @examples
#' x <- data.frame(
#'     transcriptID = "ENST00000000233",
#'     geneID = "ENSG00000004059"
#' )
#' assertAreTranscriptAnnotations(x)
assertAreTranscriptAnnotations <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    x <- as.data.frame(x)
    assert_is_subset(
        x = c("transcriptID", "geneID"),
        y = colnames(x),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}



# assertFormalCompress =========================================================
#' Assert Formal Compression
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
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



# assertFormalGene2symbol ======================================================
#' Assert Formal Gene to Symbol Mappings
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @param x Object containing gene identifiers in the row names.
#' @param genes `character` vector of gene identifiers.
#' @param gene2symbol Gene-to-symbol mappings. Must contain a `data.frame` or
#'   `NULL`.
#'
#' @examples
#' gene2symbol <- tibble(
#'     geneID = c("ENSG00000000003", "ENSG00000000005"),
#'     geneName = c("TSPAN6", "TNMD")
#' )
#' genes <- pull(gene2symbol, "geneID")
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
        assert_is_subset(rownames(x), gene2symbol[["geneID"]])
    }
}



# assertFormalInterestingGroups ================================================
#' Interesting Groups Formal Assert Check
#'
#' Prevent unwanted downstream behavior when a missing interesting group
#' is requested by the user.
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @inheritParams general
#' @param x `SummarizedExperiment`.
#'
#' @examples
#' assertFormalInterestingGroups(rse_bcb, "treatment")
#' assertFormalInterestingGroups(rse_bcb, NULL)
assertFormalInterestingGroups <- function(
    x,
    interestingGroups,
    severity = getOption("assertive.severity", "stop")
) {
    fun <- get(severity)

    # Check for SummarizedExperiment.s
    assert_is_all_of(
        x = x,
        classes = "SummarizedExperiment",
        severity = severity
    )

    # Early return clean on `NULL` value (e.g. DESeqDataSet).
    if (is.null(interestingGroups)) {
        return(invisible())
    } else {
        assert_is_character(interestingGroups)
    }

    # Obtain column data if S4 object is passed in.
    assert_is_subset(
        x = interestingGroups,
        y = colnames(colData(x)),
        severity = severity
    )

    data <- colData(x)

    # Check that interesting groups are factors.
    isFactor <- vapply(
        X = data[, interestingGroups, drop = FALSE],
        FUN = is.factor,
        FUN.VALUE = logical(1L),
        USE.NAMES = TRUE
    )
    if (!all(isFactor)) {
        invalid <- names(isFactor)[which(!isFactor)]
        fun(paste(
            "The interesting groups",
            deparse(toString(invalid)),
            "are not factor"
        ))
    }
}



# assertHasRownames ============================================================
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



#' @rdname assertHasRownames
#' @export
hasRownames <- function(x) {  # nolint
    if (is.data.frame(x)) {
        tibble::has_rownames(x)
    } else {
        assertive.properties::has_rownames(x)
    }
}



# assertIsAHeaderLevel =========================================================
#' Assert Is a Header Level
#'
#' Markdown supports header levels 1-7 (`<H1>`-`<H7>`).
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
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



# assertIsAnIntegerOrNULL ======================================================
#' Assert Is an Integer or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
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



# assertIsANumberOrNULL ========================================================
#' Assert Is a Number or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
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



# assertIsAStringOrNULL ========================================================
#' Assert Is a String or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
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



# assertIsCharacterOrNULL ======================================================
# Deprecate this function in a future release.
#' Assert Is Character Vector or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
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



# assertIsColorScaleContinuousOrNULL ===========================================
#' Assert Is Color Palette Scale Continuous or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
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



# assertIsColorScaleDiscreteOrNULL =============================================
#' Assert Is Color Palette Scale Discrete or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
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


# assertIsDataFrameOrNULL ======================================================
# Deprecate this function in a future release.
#' Assert Is Data Frame or NULL
#'
#' @note This checks for `data.frame` and will stop on `DataFrame` class.
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
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



# assertIsFillScaleContinuousOrNULL ============================================
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



# assertIsFillScaleDiscreteOrNULL ==============================================
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



# assertIsGene2symbol ==========================================================
#' Assert Is Gene to Symbol Mapping Data Frame
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @param x `data.frame` containing Ensembl gene-to-symbol mappings. Must be
#'   structured as a two column `data.frame` with "`geneID`" and "`geneName`"
#'   columns.
#'
#' @examples
#' x <- tibble(
#'     geneID = "ENSG00000000003",
#'     geneName = "TSPAN6"
#' )
#' assertIsGene2symbol(x)
assertIsGene2symbol <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    # Requiring standard data frame class.
    assert_is_data.frame(x, severity = severity)
    assert_is_non_empty(x, severity = severity)
    assert_are_identical(
        x = colnames(x),
        y = c("geneID", "geneName"),
        severity = severity
    )
    # Require rownames for standard data frame.
    if (!is_tibble(x)) {
        assertHasRownames(x, severity = severity)
    }
    # Assert that all columns are character.
    invisible(mapply(
        FUN = assert_is_character,
        x = x,
        MoreArgs = list(severity = severity),
        SIMPLIFY = FALSE
    ))
    # Assert that neither column has duplicates.
    invisible(mapply(
        FUN = assert_has_no_duplicates,
        x = x,
        MoreArgs = list(severity = severity),
        SIMPLIFY = FALSE
    ))
}



# assertIsHexColorFunctionOrNULL ===============================================
#' Assert Is Hex Color Function or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
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



# assertIsImplicitInteger ======================================================
#' Assert Is Implicit Integer
#'
#' @name assertIsImplicitInteger
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
NULL



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
    vapply(
        X = x,
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
        FUN.VALUE = logical(1L)
    )
}



# assertIsTx2gene ==============================================================
#' Assert Is Transcript-to-Gene Mapping Data
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @param x `data.frame` containing Ensembl transcript to gene identifier
#'   mappings. Must be structured as a two column `data.frame` with
#'   "transcriptID" and "geneID" columns.
#'
#' @examples
#' x <- tibble(
#'     transcriptID = "ENST00000000233",
#'     geneID = "ENSG00000004059"
#' )
#' assertIsTx2gene(x)
assertIsTx2gene <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_data.frame(x, severity = severity)
    assert_is_non_empty(x, severity = severity)
    # nocov start
    # Consider informing the user about this in a future update.
    if ("txID" %in% colnames(x)) {
        colnames(x) <- gsub("^txID$", "transcriptID", colnames(x))
    }
    # nocov end
    assert_are_identical(
        x = colnames(x),
        y = c("transcriptID", "geneID"),
        severity = severity
    )
    # Require rownames for standard data frame.
    if (!is_tibble(x)) {
        assertHasRownames(x, severity = severity)
    }
    # Assert that all columns are character.
    invisible(mapply(
        FUN = assert_is_character,
        x = x,
        MoreArgs = list(severity = severity),
        SIMPLIFY = FALSE
    ))
    # Assert that there are no duplicate transcripts.
    assert_has_no_duplicates(x[["transcriptID"]])
}



# assertAllAreURL ==============================================================
#' Assert All Are URL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @examples
#' assertAllAreURL(c(
#'     "https://www.r-project.org",
#'     "ftp://r-project.org"
#' ))
assertAllAreURL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_character(x, severity = severity)
    assert_is_non_empty(x, severity = severity)
    assert_all_are_true(isURL(x), severity = severity)
}



#' @rdname assertAllAreURL
#' @export
#' @examples
#' isURL(c(
#'     "http://www.r-project.org",
#'     "https://www.r-project.org",
#'     "ftp://r-project.org",
#'     "r-project.org"
#' ))
isURL <- function(x) {
    if (!is.character(x) || length(x) == 0L) {
        return(FALSE)
    }
    vapply(
        X = x,
        FUN = function(x) {
            grepl("^(http(s)?|ftp)\\://.+", x)
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
}
