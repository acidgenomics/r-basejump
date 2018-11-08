#' Assertions
#'
#' @name assertions
#' @keywords internal
#'
#' @param genes `character`. Gene vector, to check against corresponding object.
#' @param names `character`. Names (i.e. corresponding to rows or columns).
#'   See [base::make.names()] for more information on valid names.
#' @param object Object.
#' @param envir `environment`.
#' @param inherits `boolean`. Should the enclosing frames of the `environment`
#'   be searched?
#' @param url `character`. URL(s).
#'
#' @return Always stop on error.
NULL



# assertAllAreNonExisting ======================================================
#' Assert All Variables Are Non-Existing
#'
#' @inherit assertions
#' @export
#'
#' @param names `character`. Object names to check against environment.
#'
#' @examples
#' assertAllAreNonExisting(c("XXX", "YYY"))
assertAllAreNonExisting <- function(
    names,
    envir = parent.frame(),
    inherits = FALSE
) {
    exists <- is_existing(x = names, envir = envir, inherits = inherits)
    if (any(exists)) {
        stop(paste(
            "Already exists in environment:",
            toString(names[exists])
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
#' @inherit assertions
#' @export
#'
#' @examples
#' data(rse)
#'
#' object <- rse
#' print(object)
#'
#' genes <- object %>%
#'     SummarizedExperiment::rowData(.) %>%
#'     .[["geneName"]] %>%
#'     as.character() %>%
#'     head()
#' print(genes)
#'
#' assertAllAreUniqueGeneNames(object = object, genes = genes)
assertAllAreUniqueGeneNames <- function(object, genes) {
    assert_that(isS4(object))
    assert_is_character(genes)
    # Get all of the gene names stashed in the object.
    if (is(object, "SummarizedExperiment")) {
        requireNamespace("S4Vectors", quietly = TRUE)
        requireNamespace("SummarizedExperiment", quietly = TRUE)
        object <- object %>%
            SummarizedExperiment::rowRanges(.) %>%
            S4Vectors::mcols(.)
    }
    allGenes <- object[["geneName"]]
    assert_is_non_empty(allGenes)
    # Require that the user passed in gene names.
    assert_is_subset(genes, allGenes)
    # Check for no intersect with duplicate names.
    duplicatedGenes <- allGenes[which(duplicated(allGenes))]
    assert_are_disjoint_sets(genes, duplicatedGenes)
}



# assertAllAreURL ==============================================================
#' Assert All Are URL
#'
#' @name assertAllAreURL
#' @inherit assertions
#'
#' @examples
#' isURL(c(
#'     "http://www.r-project.org",
#'     "https://www.r-project.org",
#'     "ftp://r-project.org",
#'     "r-project.org"
#' ))
#' assertAllAreURL(c(
#'     "https://www.r-project.org",
#'     "ftp://r-project.org"
#' ))
NULL



#' @rdname assertAllAreURL
#' @export
isURL <- function(url) {
    if (
        !is.character(url) ||
        length(url) == 0L
    ) {
        return(FALSE)
    }
    vapply(
        X = url,
        FUN = function(url) {
            grepl("^(http(s)?|ftp)\\://.+", url)
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
}



#' @rdname assertAllAreURL
#' @export
assertAllAreURL <- function(url) {
    assert_is_character(url)
    assert_is_non_empty(url)
    assert_all_are_true(isURL(url))
}



# assertAreValidNames ==========================================================
#' Assert Are Valid Names
#'
#' @name assertAreValidNames
#' @inherit assertions
#'
#' @examples
#' data(rse)
#' gr <- SummarizedExperiment::rowRanges(rse)
#'
#' # Dots (periods) and underscores are valid.
#' validNames(c("sample.1", "sample_1"))
#'
#' # Can't begin with a number.
#' validNames("293cells")
#'
#' # Spaces, dashes (hyphens), and other non-alphanumerics aren't valid.
#' validNames("sample 1")
#' validNames("cell-AAAAAAAA")
#' validNames("GFP+")
#'
#' assertAreValidNames(c("sample1", "sample2"))
#' assertHasValidNames(gr)
#'
#' # Check rows and columns (dimnames) in a single call.
#' validDimnames(datasets::iris)    # TRUE
#' validDimnames(datasets::mtcars)  # FALSE
#' assertHasValidDimnames(rse)
NULL



#' @rdname assertAreValidNames
#' @export
validNames <- function(names) {
    if (!is.character(names)) {
        FALSE
    } else {
        identical(names, make.names(names, unique = TRUE))
    }
}



#' @rdname assertAreValidNames
#' @export
assertAreValidNames <- function(names) {
    assert_that(validNames(names))
}



#' @rdname assertAreValidNames
#' @export
assertHasValidNames <- function(object) {
    assert_has_names(object)
    invisible(lapply(names(object), assertAllAreValidNames))
}



#' @rdname assertAreValidNames
#' @export
validDimnames <- function(object) {
    # Error if object doesn't support dim.
    assert_that(!is.null(dim(object)))

    # Return TRUE if there are no names.
    if (!has_dimnames(object)) {
        return(TRUE)
    }

    # Check rows.
    if (hasRownames(object)) {
        validRows <- validNames(rownames(object))
    } else {
        validRows <- TRUE
    }
    assert_is_a_bool(validRows)

    # Check columns.
    if (has_colnames(object)) {
        validCols <- validNames(colnames(object))
    } else {
        validCols <- TRUE
    }
    assert_is_a_bool(validCols)

    all(c(validRows, validCols))
}



#' @rdname assertAreValidNames
#' @export
assertHasValidDimnames <- function(object) {
    assert_has_dimnames(object)
    assert_all_are_true(validDimnames(object))
}



# assertFormalCompress =========================================================
#' Assert Formal Compression
#'
#' @inherit assertions
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



# assertFormalGene2Symbol ======================================================
#' Assert Formal Gene-to-Symbol Mappings
#'
#' @inherit assertions
#' @export
#'
#' @param object Object class supporting `rownames`. All rownames in this object
#'   must intersect with the rownames defined in the `gene2symbol` argument.
#' @param genes `character`. Gene identifiers. Note that gene names (symbols)
#'   are also supported, but not recommended if the stable IDs can be easily
#'   provided instead.
#' @param gene2symbol `Gene2Symbol`. Gene-to-symbol mappings. Must contain
#'   `geneID` and `geneName` columns, with rownames defined. All of the `object`
#'   rownames must be defined here, otherwise the function will error.
#'
#' @examples
#' DataFrame <- S4Vectors::DataFrame
#'
#' object <- DataFrame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(object)
#'
#' gene2symbol <- Gene2Symbol(
#'     object = DataFrame(
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
#' assertFormalGene2Symbol(
#'     object = object,
#'     genes = geneIDs,
#'     gene2symbol = gene2symbol
#' )
#' assertFormalGene2Symbol(
#'     object = object,
#'     genes = geneNames,
#'     gene2symbol = gene2symbol
#' )
assertFormalGene2Symbol <- function(
    object,
    genes,
    gene2symbol
) {
    assertHasRownames(object)
    assert_is_character(genes)
    assert_is_non_empty(genes)
    assert_is_all_of(gene2symbol, "Gene2Symbol")
    assert_are_identical(
        x = nrow(object),
        y = nrow(gene2symbol)
    )
    if (is.null(rownames(gene2symbol))) {
        rownames(gene2symbol) <- rownames(object)
    }
    # Map genes to object rownames, using gene2symbol.
    rownames <- mapGenesToRownames(object = gene2symbol, genes = genes)
    assert_is_subset(rownames, rownames(object))
    invisible()
}



# assertFormalInterestingGroups ================================================
#' Interesting Groups Formal Assert Check
#'
#' Prevent unwanted downstream behavior when a missing interesting group
#' is requested by the user.
#'
#' @inherit assertions
#' @export
#'
#' @param object S4 class object.
#' @param interestingGroups `character`. Interesting groups.
#'
#' @examples
#' data(rse)
#' assertFormalInterestingGroups(rse, "treatment")
#' assertFormalInterestingGroups(rse, NULL)
assertFormalInterestingGroups <- function(object, interestingGroups) {
    assert_that(isS4(object))
    data <- sampleData(object)

    # Check `interestingGroups` argument.
    if (is.null(interestingGroups)) {
        # Early return clean on `NULL` value (e.g. DESeqDataSet).
        return(invisible())
    } else {
        # Otherwise, require that `interestingGroups` is a character.
        assert_is_character(interestingGroups)
    }

    # Check intersection with sample data.
    assert_is_subset(interestingGroups, colnames(data))

    # Check that interesting groups columns are factors.
    invisible(lapply(
        X = data[, interestingGroups, drop = FALSE],
        FUN = assert_is_factor
    ))
}



# assertHasAggregateInfo =======================================================
#' Assert Has Aggregate Information
#'
#' Determine whether an object can be aggregated automatically.
#'
#' @name assertHasAggregateInfo
#' @inherit assertions
NULL



#' @rdname assertHasAggregateInfo
#' @export
hasAggregateInfo <- function(object) {
    "aggregate" %in% colnames(object)
}



#' @rdname assertHasAggregateInfo
#' @export
assertHasAggregateInfo <- function(object) {
    assert_that(isTRUE(hasAggregateInfo))
}



# assertHasRownames ============================================================
#' Assert Has Rownames
#'
#' A stricter alternative to the assertive version that works properly with
#' data frames.
#'
#' @name assertHasRownames
#' @inherit assertions
#'
#' @examples
#' object <- S4Vectors::DataFrame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(object)
#' assertHasRownames(object)
NULL



# `tibble::has_rownames()` appears to be more consistent than
# `assertive.properties::has_rownames()` for `DataFrame` and `tbl_df` class.
#' @rdname assertHasRownames
#' @export
hasRownames <- function(object) {
    if (
        is(object, "data.table") ||
        is(object, "tbl_df")
    ) {
        # Check for rowname column for classes that inherit data.frame but
        # don't allow rownames to be set.
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



#' @rdname assertHasRownames
#' @export
assertHasRownames <- function(object) {
    assert_that(hasRownames(object))
    if (!is(object, "tbl_df")) {
        assert_are_disjoint_sets(
            x = rownames(object),
            y = as.character(seq_len(nrow(object)))
        )
    }
}



# assertIsAlpha ================================================================
#' Assert Is Alpha Level
#'
#' An alpha level must be between 0 and 1, but not equal either 0 or 1.
#'
#' @inherit assertions
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
#' @inherit assertions
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
#' @inherit assertions
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
#' @inherit assertions
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
#' @inherit assertions
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
#' @inherit assertions
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



# assertIsDir ==================================================================
#' Assert Is Directory
#'
#' @inherit assertions
#' @export
#'
#' @examples
#' assertIsDir("~")
assertIsDir <- function(object) {
    assert_is_a_string(object)
    assert_all_are_dirs(object)
}



# assertIsFile =================================================================
#' Assert Is File
#'
#' @inherit assertions
#' @export
#'
#' @examples
#' assertIsFile(system.file("extdata/example.rds", package = "basejump"))
assertIsFile <- function(object) {
    assert_is_a_string(object)
    assert_all_are_dirs(object)
}



# assertIsFillScaleContinuousOrNULL ============================================
#' Assert Is Fill Palette Scale Continuous or NULL
#'
#' @inherit assertions
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
#' @inherit assertions
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



# assertIsHeaderLevel ==========================================================
#' Assert Is Markdown Header Level
#'
#' Markdown supports header levels 1-7 (`<H1>`-`<H7>`).
#'
#' @inherit assertions
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
#' @inherit assertions
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
#' @inherit assertions
#'
#' @examples
#' isImplicitInteger(list(1, 1L, 1.1, "XXX"))
#' assertIsImplicitInteger(c(1, 2))
#' assertIsImplicitIntegerOrNULL(c(1, 2))
#' assertIsImplicitIntegerOrNULL(NULL)
#'
#' isAnImplicitInteger(1)
#' assertIsAnImplicitInteger(1)
#' assertIsAnImplicitIntegerOrNULL(1)
#' assertIsAnImplicitIntegerOrNULL(NULL)
NULL



#' @rdname assertIsImplicitInteger
#' @export
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



#' @rdname assertIsImplicitInteger
#' @export
assertIsImplicitInteger <- function(object) {
    assert_all_are_true(isImplicitInteger(object))
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsImplicitIntegerOrNULL <- function(object) {
    assert_any_are_true(c(
        isImplicitInteger(object),
        is.null(object)
    ))
}



#' @rdname assertIsImplicitInteger
#' @export
isAnImplicitInteger <- function(object) {
    if (!is_a_number(object)) {
        return(FALSE)
    }
    isImplicitInteger(object)
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsAnImplicitInteger <- function(object) {
    assert_that(isAnImplicitInteger(object))
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsAnImplicitIntegerOrNULL <- function(object) {
    assert_that(any(isAnImplicitInteger(object), is.null(object)))
}
