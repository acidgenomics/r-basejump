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
#' @return Always stop on error.
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
#' genes <- head(as.character(rowData(rse_small)[["geneName"]]))
#' print(genes)
#' assertAllAreUniqueGeneNames(object = object, genes = genes)
assertAllAreUniqueGeneNames <- function(object, genes) {
    assert_is_any_of(object, classes = c("gene2symbol", "SummarizedExperiment"))
    assert_is_character(genes)
    # Get all of the gene names stashed in the object.
    if (is(object, "SummarizedExperiment")) {
        allGenes <- mcols(rowRanges(object))[["geneName"]]
    } else {
        allGenes <- object[["geneName"]]
    }
    assert_is_non_empty(allGenes)
    # Require that the user passed in gene names.
    assert_is_subset(genes, allGenes)
    # Check for no intersect with duplicate names.
    duplicatedGenes <- allGenes[which(duplicated(allGenes))]
    assert_are_disjoint_sets(genes, duplicatedGenes)
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
    assert_all_are_non_missing_nor_empty_character(object)
    assert_has_no_duplicates(object)
    assert_all_are_true(validNames(object))
}



#' @rdname assertAllAreValidNames
#' @export
assertHasValidDimnames <- function(object) {
    assert_has_dimnames(object)
    assert_all_are_true(validDimnames(object))
}



#' @rdname assertAllAreValidNames
#' @export
assertHasValidNames <- function(object) {
    if (has_dims(object)) {
        stop("Use `assertHasValidDimnames()` instead.")
    }
    assert_has_names(object)
    invisible(lapply(names(object), assertAllAreValidNames))
}



#' @rdname assertAllAreValidNames
#' @export
#' @examples
#' validDimnames(datasets::mtcars)
validDimnames <- function(object) {
    if (is.null(dim(object))) {
        stop("Object does not support `dim()`.")
    } else if (!has_dimnames(object)) {
        c(TRUE, TRUE)
    } else {
        vapply(
            X = lapply(
                X = dimnames(object),
                FUN = function(x) {
                    # Return `FALSE` on duplicates.
                    if (any(duplicated(x))) {
                        FALSE
                    } else {
                        validNames(x)
                    }
                }
            ),
            FUN = all,
            FUN.VALUE = logical(1L)
        )
    }
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
        FALSE
    } else if (is.null(object)) {
        TRUE
    } else {
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
    assert_is_subset(c("geneID", "geneName"), colnames(df))
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
    assert_is_subset(c("transcriptID", "geneID"), colnames(df))
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
    assert_is_any_of(object, classes = c("character", "logical"))
    if (is.character(object)) {
        assert_is_a_string(object)
        assert_is_subset(object, c("bzip2", "gzip", "xz"))
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
#' @param object Object class supporting `rownames`. All rownames in this object
#'   must intersect with the rownames defined in the `gene2symbol` argument.
#' @param genes `character`. Gene identifiers. Note that gene names (symbols)
#'   are also supported, but not recommended if the stable IDs can be easily
#'   provided instead.
#' @param gene2symbol `gene2symbol`. Gene-to-symbol mappings. Must contain
#'   `geneID` and `geneName` columns, with rownames defined. All of the `object`
#'   rownames must be defined here, otherwise the function will error.
#'
#' @examples
#' object <- DataFrame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(object)
#'
#' gene2symbol <- new(
#'     Class = "gene2symbol",
#'     DataFrame(
#'         geneID = c("ENSG00000000003", "ENSG00000000005"),
#'         geneName = c("TSPAN6", "TNMD"),
#'         row.names = rownames(object)
#'     )
#' )
#' print(gene2symbol)
#'
#' geneIDs <- gene2symbol[["geneID"]]
#' print(geneIDs)
#'
#' geneNames <- gene2symbol[["geneName"]]
#' print(geneNames)
#'
#' assertFormalGene2symbol(
#'     object = object,
#'     genes = geneIDs,
#'     gene2symbol = gene2symbol
#' )
#' assertFormalGene2symbol(
#'     object = object,
#'     genes = geneNames,
#'     gene2symbol = gene2symbol
#' )
assertFormalGene2symbol <- function(
    object,
    genes,
    gene2symbol
) {
    assertHasRownames(object)
    assert_is_character(genes)
    assert_is_all_of(gene2symbol, "gene2symbol")
    # Require that all rownames of object are defined in gene2symbol.
    assert_is_subset(rownames(object), rownames(gene2symbol))
    # Check to ensure the user defined genes map to the rownames of the object.
    rownames <- mapGenesToRownames(object = gene2symbol, genes = genes)
    assert_is_non_empty(rownames)
    assert_is_subset(rownames, rownames(object))
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
#' assertFormalInterestingGroups(rse_small, "treatment")
#' assertFormalInterestingGroups(rse_small, NULL)
assertFormalInterestingGroups <- function(object, interestingGroups) {
    # Always require SummarizedExperiment for object.
    assert_is_all_of(object, classes = "SummarizedExperiment")

    # Check `interestingGroups` argument.
    if (is.null(interestingGroups)) {
        # Early return clean on `NULL` value (e.g. DESeqDataSet).
        return(invisible())
    } else {
        # Otherwise, require that `interestingGroups` is a character.
        assert_is_character(interestingGroups)
    }

    # Check intersection with column data.
    assert_is_subset(interestingGroups, colnames(colData(object)))

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
    if (!is(object, "tbl_df")) {
        assert_are_disjoint_sets(
            x = rownames(object),
            y = as.character(seq_len(nrow(object)))
        )
    }
}



# `tibble::has_rownames()` may be more consistent than
# `assertive.properties::has_rownames()` for `DataFrame` and `tbl_df` class.
#' @rdname assertHasRownames
#' @export
hasRownames <- function(object) {
    if (is(object, "tbl_df")) {
        "rowname" %in% colnames(object)
    } else if (identical(
        x = as.character(rownames(object)),
        y = as.character(seq_len(nrow(object)))
    )) {
        # Check for numeric rownames that match rows.
        FALSE
    } else {
        has_rownames(object)
    }
}



# assertIsAlpha ================================================================
#' Assert Is Alpha Level
#'
#' An alpha level must be between 0 and 1, but not equal either 0 or 1.
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @examples
#' assertIsAlpha(0.05)
assertIsAlpha <- function(object) {
    assert_is_a_number(object)
    assert_all_are_in_open_range(object, lower = 0L, upper = 1L)
    if (object > 0.1) {
        warning("An alpha level above 0.1 (10%) is not recommended.")
    }
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
    assert_is_any_of(object, classes = c("integer", "NULL"))
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
    assert_is_any_of(object, classes = c("numeric", "NULL"))
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
    assert_is_any_of(object, classes = c("character", "NULL"))
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
    assert_is_any_of(object, classes = c("ScaleContinuous", "NULL"))
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleContinuous")
        )
        # Note that this has to match the British spelling.
        assert_are_identical(object[["aesthetics"]], "colour")
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
    assert_is_any_of(object, classes = c("ScaleDiscrete", "NULL"))
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleDiscrete")
        )
        # Note that this has to match the British spelling.
        assert_are_identical(object[["aesthetics"]], "colour")
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
    assert_is_any_of(object, classes = c("ScaleContinuous", "NULL"))
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleContinuous")
        )
        assert_are_identical(object[["aesthetics"]], "fill")
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
    assert_is_any_of(object, classes = c("ScaleDiscrete", "NULL"))
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleDiscrete")
        )
        assert_are_identical(object[["aesthetics"]], "fill")
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
        # Remove data.frame in a future update.
        # This can cause validity checks on old bcbio objects to fail otherwise.
        classes = c("DataFrame", "tbl_df", "data.frame")
    )
    assert_is_non_empty(object)
    assert_are_identical(colnames(object), c("geneID", "geneName"))
    # Require rownames for standard data frame.
    if (!is(object, "tbl_df")) {
        assertHasRownames(object)
    }
    # Assert that all columns are character.
    invisible(lapply(object, assert_is_character))
    # Assert that neither column has duplicates.
    invisible(lapply(object, assert_has_no_duplicates))
}



# assertIsHeaderLevel ==========================================================
#' Assert Is Markdown Header Level
#'
#' Markdown supports header levels 1-7 (`<H1>`-`<H7>`).
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#' @export
#'
#' @examples
#' assertIsHeaderLevel(1L)
assertIsHeaderLevel <- function(object) {
    assert_is_a_number(object)
    assert_is_subset(as.integer(object), seq_len(7L))
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
    assert_is_any_of(object, classes = c("function", "NULL"))
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
#'     geneID = "ENSG00000004059",
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
    assert_is_any_of(
        x = object,
        # Remove data.frame in a future update.
        # This can cause validity checks on old bcbio objects to fail otherwise.
        classes = c("DataFrame", "tbl_df", "data.frame")
    )
    assert_is_non_empty(object)
    # nocov start
    if ("txID" %in% colnames(object)) {
        # Consider warning here in a future update.
        # "Use `transcript` instead of `tx`"
        colnames(object) <- gsub("^txID$", "transcriptID", colnames(object))
    }
    # nocov end
    assert_are_identical(colnames(object), c("transcriptID", "geneID"))
    # Require rownames for DataFrame.
    if (!is(object, "tbl_df")) {
        assertHasRownames(object)
    }
    # Assert that all columns are character.
    invisible(lapply(object, assert_is_character))
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
    if (!is.character(object) || !has_length(object)) {
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
