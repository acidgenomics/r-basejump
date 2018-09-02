#' Assert Checks
#'
#' @name assert
#' @keywords internal
#'
#' @param object Object.
#' @param envir `environment`.
#' @param inherits `boolean`. Should the enclosing frames of the `environment`
#'   be searched?
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
#' @param object `character`. Variable names to check in `environment`.
#'
#' @examples
#' assertAllAreNonExisting(c("XXX", "YYY"))
assertAllAreNonExisting <- function(
    object,
    envir = parent.frame(),
    inherits = FALSE
) {
    exists <- is_existing(object, envir = envir, inherits = inherits)
    if (any(exists)) {
        stop(paste(
            "Already exists in environment:",
            toString(object[exists])
        ))
    }
}



# assertAllAreUniqueGeneNames ==================================================
#' Assert All Are Unique Gene Names
#'
#' This assert check determines if a user-defined gene name query is using only
#' unique (non-amgibuous) symbols. It is designed to be used for gene plotting
#' particularly when performing single-cell RNA-seq marker analysis.
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @param object `SummarizedExperiment`.
#' @param genes `character`. Input vector to check against definitions in the
#'   corresponding `SummarizedExperiment`.
#'
#' @examples
#' object <- rse_small
#' print(object)
#' genes <- head(as.character(rowData(rse_small)$geneName))
#' print(genes)
#' assertAllAreUniqueGeneNames(object = object, genes = genes)
assertAllAreUniqueGeneNames <- function(object, genes) {
    assert_is_all_of(
        x = object,
        classes = "SummarizedExperiment"
    )
    assert_is_character(genes)
    # Get all of the gene names stashed in the object.
    allGenes <- mcols(rowRanges(object))[["geneName"]]
    assert_is_non_empty(allGenes)
    # Require that the user passed in gene names.
    assert_is_subset(
        x = genes,
        y = allGenes
    )
    # Check for no intersect with duplicate names.
    duplicatedGenes <- allGenes[which(duplicated(allGenes))]
    assert_are_disjoint_sets(
        x = genes,
        y = duplicatedGenes
    )
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
assertAllAreValidNames <- function(object) {
    assert_is_character(object)
    assert_is_non_empty(object)
    assert_all_are_true(validNames(object))
}



#' @rdname assertAllAreValidNames
#' @export
assertHasValidDimnames <- function(object) {
    assert_has_dimnames(object)
    invisible(lapply(
        X = dimnames(object),
        FUN = assertAllAreValidNames
    ))
}



#' @rdname assertAllAreValidNames
#' @export
assertHasValidNames <- function(object) {
    if (has_dims(object)) {
        stop("Use `assertHasValidDimnames()` instead")
    }
    assert_has_names(object)
    invisible(lapply(
        X = names(object),
        FUN = assertAllAreValidNames
    ))
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
validNames <- function(object) {
    if (!is.atomic(object)) {
        return(FALSE)
    }
    vapply(
        X = object,
        FUN = function(object) {
            # Note that we're enforcing unique values here.
            identical(
                x = as.character(object),
                y = make.names(object, unique = TRUE)
            )
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
}



# assertAreGeneAnnotations =====================================================
#' Assert Are Gene Annotations
#'
#' Must contain `geneID` and `geneName` columns. Does not need to contain
#' rownames, so `tibble` class is supported.
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @param object Object that can be coerced to `data.frame`.
#'
#' @examples
#' object <- tibble(
#'     geneID = "ENSG00000000003",
#'     geneName = "TSPAN6"
#' )
#' assertAreGeneAnnotations(object)
assertAreGeneAnnotations <- function(object) {
    df <- as.data.frame(object)
    assert_is_subset(
        x = c("geneID", "geneName"),
        y = colnames(df)
    )
    assert_is_non_empty(df)
}



# assertAreTranscriptAnnotations ===============================================
#' Assert Are Ensembl Transcript Annotations
#'
#' Must contain `transcriptID` and `transcriptName` columns. Does not need to
#' contain rownames, so `tibble` class is supported.
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @param object Object that can be coerced to `data.frame`.
#'
#'
#' @examples
#' object <- tibble(
#'     transcriptID = "ENST00000000233",
#'     geneID = "ENSG00000004059"
#' )
#' assertAreTranscriptAnnotations(object)
assertAreTranscriptAnnotations <- function(object) {
    df <- as.data.frame(object)
    assert_is_subset(
        x = c("transcriptID", "geneID"),
        y = colnames(df)
    )
    assert_is_non_empty(object)
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
assertFormalCompress <- function(object) {
    assert_is_any_of(
        x = object,
        classes = c("character", "logical")
    )
    if (is.character(object)) {
        assert_is_a_string(object)
        assert_is_subset(
            x = object,
            y = c("bzip2", "gzip", "xz")
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
#' @param object Object containing gene identifiers in `rownames`. Cannot
#'   contain gene names, or the check will fail.
#' @param genes `character`. Gene identifiers.
#' @param gene2symbol `DataFrame`, `tbl_df`, or `NULL`. Gene-to-symbol mappings.
#'
#' @examples
#' gene2symbol <- tibble(
#'     geneID = c("ENSG00000000003", "ENSG00000000005"),
#'     geneName = c("TSPAN6", "TNMD")
#' )
#' genes <- pull(gene2symbol, "geneID")
#' object <- DataFrame(
#'     "sample_1" = c(1L, 2L),
#'     "sample_2" = c(3L, 4L),
#'     row.names = genes
#' )
#' assertFormalGene2symbol(object, genes, gene2symbol)
assertFormalGene2symbol <- function(
    object,
    genes,
    gene2symbol
) {
    assertHasRownames(object)
    assert_is_any_of(
        x = genes,
        classes = c("character", "NULL")
    )
    if (is.character(genes)) {
        assert_is_subset(
            x = genes,
            y = rownames(object)
        )
    }
    assert_is_any_of(
        x = gene2symbol,
        classes = c("DataFrame", "tbl_df", "NULL")
    )
    if (!is.null(gene2symbol)) {
        assertIsGene2symbol(gene2symbol)
        assert_is_subset(
            x = rownames(object),
            y = gene2symbol[["geneID"]]
        )
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
#' @param object `SummarizedExperiment`.
#'
#' @examples
#' assertFormalInterestingGroups(rse_bcb, "treatment")
#' assertFormalInterestingGroups(rse_bcb, NULL)
assertFormalInterestingGroups <- function(object, interestingGroups) {
    # Always require SummarizedExperiment for object.
    assert_is_all_of(
        x = object,
        classes = "SummarizedExperiment"
    )

    # Check `interestingGroups` argument.
    if (is.null(interestingGroups)) {
        # Early return clean on `NULL` value (e.g. DESeqDataSet).
        return(invisible())
    } else {
        # Otherwise, require that `interestingGroups` is a character.
        assert_is_character(interestingGroups)
    }

    # Check intersection with column data.
    assert_is_subset(
        x = interestingGroups,
        y = colnames(colData(object))
    )

    # Check that interesting groups columns are factors.
    invisible(lapply(
        X = colData(object)[, interestingGroups, drop = FALSE],
        FUN = assert_is_factor
    ))
}



# assertHasRownames ============================================================
#' Assert Has Rownames
#'
#' A stricter alternative to the assertive version that works properly with
#' data frames.
#'
#' @note `tibble::has_rownames()` is more consistent than
#'   `assertive.properties::has_rownames()` for `DataFrame` and `tbl_df` class.
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @examples
#' object <- DataFrame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(object)
#' assertHasRownames(object)
assertHasRownames <- function(object) {
    assert_all_are_true(hasRownames(object))
    assert_are_disjoint_sets(
        x = rownames(object),
        y = as.character(seq_len(nrow(object)))
    )
}



#' @rdname assertHasRownames
#' @export
hasRownames <- function(object) {
    if (is.data.frame(object)) {
        tibble::has_rownames(object)
    } else {
        assertive.properties::has_rownames(object)
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
assertIsAHeaderLevel <- function(object) {
    assert_is_a_number(object)
    assert_is_subset(
        x = as.integer(object),
        y = seq(1L:7L)
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
assertIsAnIntegerOrNULL <- function(object) {
    assert_is_any_of(
        x = object,
        classes = c("integer", "NULL")
    )
    if (is.integer(object)) {
        assert_is_an_integer(object)
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
assertIsANumberOrNULL <- function(object) {
    assert_is_any_of(
        x = object,
        classes = c("numeric", "NULL")
    )
    if (is.numeric(object)) {
        assert_is_a_number(object)
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
assertIsAStringOrNULL <- function(object) {
    assert_is_any_of(
        x = object,
        classes = c("character", "NULL")
    )
    if (is.character(object)) {
        assert_is_a_string(object)
    }
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
assertIsColorScaleContinuousOrNULL <- function(object) {
    assert_is_any_of(
        x = object,
        classes = c("ScaleContinuous", "NULL")
    )
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleContinuous")
        )
        assert_are_identical(
            x = object[["aesthetics"]],
            y = "colour"
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
assertIsColorScaleDiscreteOrNULL <- function(object) {
    assert_is_any_of(
        x = object,
        classes = c("ScaleDiscrete", "NULL")
    )
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleDiscrete")
        )
        assert_are_identical(
            x = object[["aesthetics"]],
            y = "colour"
        )
    }
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
assertIsFillScaleContinuousOrNULL <- function(object) {
    assert_is_any_of(
        x = object,
        classes = c("ScaleContinuous", "NULL")
    )
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleContinuous")
        )
        assert_are_identical(
            x = object[["aesthetics"]],
            y = "fill"
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
assertIsFillScaleDiscreteOrNULL <- function(object) {
    assert_is_any_of(
        x = object,
        classes = c("ScaleDiscrete", "NULL")
    )
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleDiscrete")
        )
        assert_are_identical(
            x = object[["aesthetics"]],
            y = "fill"
        )
    }
}



# assertIsGene2symbol ==========================================================
#' Assert Is Gene to Symbol Mapping Data Frame
#'
#' @note Standard `data.frame` class is not supported. Use either `DataFrame`
#'   or `tbl_df` class.
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @param object `DataFrame` or `tbl_df`. Must contain "`geneID`" and
#'   "`geneName`" columns. If `DataFrame`, must also have `rownames` set.
#'
#' @examples
#' # DataFrame ====
#' object <- DataFrame(
#'     geneID = "ENSG00000000003",
#'     geneName = "TSPAN6",
#'     row.names = "ENSG00000000003"
#' )
#' assertIsGene2symbol(object)
#'
#' # tibble ====
#' object <- tibble(
#'     geneID = "ENSG00000000003",
#'     geneName = "TSPAN6"
#' )
#' assertIsGene2symbol(object)
assertIsGene2symbol <- function(object) {
    # Requiring standard data frame class.
    assert_is_any_of(
        x = object,
        classes = c("DataFrame", "tbl_df")
    )
    assert_is_non_empty(object)
    assert_are_identical(
        x = colnames(object),
        y = c("geneID", "geneName")
    )
    # Require rownames for standard data frame.
    if (!is_tibble(object)) {
        assertHasRownames(object)
    }
    # Assert that all columns are character.
    invisible(lapply(
        X = object,
        FUN = assert_is_character
    ))
    # Assert that neither column has duplicates.
    invisible(lapply(
        X = object,
        FUN = assert_has_no_duplicates
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
assertIsHexColorFunctionOrNULL <- function(object) {
    assert_is_any_of(
        x = object,
        classes = c("function", "NULL")
    )
    if (is.function(object)) {
        colors <- object(2L)
        assert_is_character(colors)
        if (!all(is_hex_color(colors))) {
            # viridis adds "FF" to the end of hex colors.
            # Attempt to fix before running hex check.
            colors <- gsub("^(#[A-Z0-9]{6})[A-Z0-9]{2}$", "\\1", colors)
        }
        assert_all_are_hex_colors(colors)
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
assertIsAnImplicitInteger <- function(object) {
    assert_all_are_true(isAnImplicitInteger(object))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsAnImplicitIntegerOrNULL(1)
#' assertIsAnImplicitIntegerOrNULL(NULL)
assertIsAnImplicitIntegerOrNULL <- function(object) {
    assert_all_are_true(any(isAnImplicitInteger(object), is.null(object)))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsImplicitInteger(c(1, 2))
assertIsImplicitInteger <- function(object) {
    assert_all_are_true(isImplicitInteger(object))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsImplicitIntegerOrNULL(c(1, 2))
#' assertIsImplicitIntegerOrNULL(NULL)
assertIsImplicitIntegerOrNULL <- function(object) {
    assert_all_are_true(any(isImplicitInteger(object), is.null(object)))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' isAnImplicitInteger(1)
isAnImplicitInteger <- function(object) {
    if (!is_a_number(object)) {
        return(FALSE)
    }
    isImplicitInteger(object)
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' isImplicitInteger(list(1, 1L, 1.1, "XXX"))
isImplicitInteger <- function(object) {
    vapply(
        X = object,
        FUN = function(object) {
            if (!is.numeric(object)) {
                return(FALSE)
            }
            if (is.integer(object)) {
                return(TRUE)
            }
            isTRUE(all.equal(
                target = as.integer(object),
                current = object,
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
#' @inherit assertIsGene2symbol
#' @export
#'
#' @param object `DataFrame` or `tbl_df` containing transcript-to-gene
#'   identifier mappings. Must contain the columns "`transcriptID`" and
#'   "`geneID`". If `DataFrame`, must also have `rownames` set.
#'
#' @examples
#' # DataFrame ====
#' object <- DataFrame(
#'     transcriptID = "ENST00000000233",
#'     geneID = "ENSG00000004059"
#'     row.names = "ENST00000000233"
#' )
#'
#' # tibble ====
#' object <- tibble(
#'     transcriptID = "ENST00000000233",
#'     geneID = "ENSG00000004059"
#' )
#' assertIsTx2gene(object)
assertIsTx2gene <- function(object) {
    assert_is_all_of(
        x = object,
        classes = c("DataFrame", "tbl_df")
    )
    assert_is_non_empty(object)
    # nocov start
    if ("txID" %in% colnames(object)) {
        warning("Use `transcript` instead of `tx`")
        colnames(object) <- gsub("^txID$", "transcriptID", colnames(object))
    }
    # nocov end
    assert_are_identical(
        x = colnames(object),
        y = c("transcriptID", "geneID")
    )
    # Require rownames for DataFrame.
    if (!is_tibble(object)) {
        assertHasRownames(object)
    }
    # Assert that all columns are character.
    invisible(lapply(
        X = object,
        FUN = assert_is_character
    ))
    # Assert that there are no duplicate transcripts.
    assert_has_no_duplicates(object[["transcriptID"]])
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
assertAllAreURL <- function(object) {
    assert_is_character(object)
    assert_is_non_empty(object)
    assert_all_are_true(isURL(object))
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
isURL <- function(object) {
    if (!is.character(object) || length(object) == 0L) {
        return(FALSE)
    }
    vapply(
        X = object,
        FUN = function(object) {
            grepl("^(http(s)?|ftp)\\://.+", object)
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
}
