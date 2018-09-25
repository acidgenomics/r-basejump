# FIXME Check SCE method.



#' Combine Multiple Objects
#'
#' @note We're attempting to make this as strict as possible, requiring:
#'
#' - Rows (genes) across objects must be identical.
#' - [rowRanges()] and/or [rowData()] metadata must be identical.
#' - [colData()] must contain the same columns.
#' - Specific metadata must be identical (see `metadata` parameter).
#'
#' @name combine
#' @author Michael Steinbaugh
#' @importFrom BiocGenerics combine
#' @export
#'
#' @inheritParams BiocGenerics::combine
#'
#' @seealso
#' - [BiocGenerics::combine()].
#' - `help("merge.Matrix", "Matrix.utils")`.
#'
#' @return `SummarizedExperiment`.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- rse_small
#' colnames(x)
#' colData(x)
#'
#' # Create a copy of our minimal example.
#' y <- x
#' colnames(y) <- paste0("sample", seq(from = 5L, to = 8L))
#' colnames(y)
#' colData(y)
#'
#' # Combine two SummarizedExperiment objects.
#' c <- combine(x, y)
#' print(c)
#' colnames(c)
#' colData(c)
#'
#' # SingleCellExperiment ====
#' x <- sce_small
#' head(colnames(x))
#' sampleData(x)
#'
#' # Here we're faking a distinct replicate, just as an example.
#' y <- x
#' # Increase the cell ID numbers.
#' cells <- colnames(y) %>%
#'     sub("cell", "", .) %>%
#'     as.integer() %>%
#'     `+`(ncol(y)) %>%
#'     paste0("cell", .)
#' colnames(y) <- cells
#' head(colnames(y))
#' # Increase the sample ID numbers.
#' y$sampleID <- gsub("1$", "3", y$sampleID)
#' y$sampleID <- gsub("2$", "4", y$sampleID)
#' sampleData(y)
#'
#' # Combine two SingleCellExperiment objects.
#' c <- combine(x, y)
#' print(c)
#' sampleNames(c)
NULL



# FIXME Move these assert checks to SE method.
# assert_are_identical(class(x), class(y))
# assert_are_set_equal(
#     x = colnames(sampleData(x)),
#     y = colnames(sampleData(y))
# )
# assert_is_character(metadata)
.combine.SE <-  # nolint
    function(x, y) {
        # Coerce the objects to SummarizedExperiment.
        # Keep as RSE if the data is ranged.
        assert_are_identical(class(x), class(y))
        if (is(x, "RangedSummarizedExperiment")) {
            Class <- "RangedSummarizedExperiment"  # nolint
        } else {
            Class <- "SummarizedExperiment"  # nolint
        }
        x <- as(object = x, Class = Class)
        y <- as(object = y, Class = Class)

        # Currently we're being strict and requiring that the rows (features)
        # are identical, otherwise zero counts may be misleading.
        assert_are_identical(rownames(x), rownames(y))

        # Require that there are no duplicate cells.
        assert_are_disjoint_sets(colnames(x), colnames(y))

        # Require specific metadata to be identical, if defined.
        metadata <- c(
            "dataVersions",
            "gffFile",
            "interestingGroups",
            "level",
            "organism",
            "pipeline",
            "programVersions",
            "umiType",
            "version"
        )
        assert_are_identical(
            x = metadata(x)[metadata],
            y = metadata(y)[metadata]
        )

        # Counts ---------------------------------------------------------------
        # Check that count matrices are identical format, then combine.
        assert_are_identical(
            x = class(counts(x)),
            y = class(counts(y))
        )
        counts <- cbind(counts(x), counts(y))

        # Row data -------------------------------------------------------------
        # Require that the gene annotations are identical.
        if (is(x, "RangedSummarizedExperiment")) {
            assert_are_identical(
                x = rowRanges(x),
                y = rowRanges(y)
            )
            rowRanges <- rowRanges(x)
            rowData <- NULL
        } else {
            assert_are_identical(
                x = rowData(x),
                y = rowData(y)
            )
            rowData <- rowData(x)
            rowRanges <- NULL
        }

        # Column data ----------------------------------------------------------
        assert_are_set_equal(
            x = colnames(colData(x)),
            y = colnames(colData(y))
        )
        cols <- intersect(
            x = colnames(colData(x)),
            y = colnames(colData(y))
        )
        colData <- rbind(
            colData(x)[, cols, drop = FALSE],
            colData(y)[, cols, drop = FALSE]
        )

        # Metadata -------------------------------------------------------------
        metadata <- metadata(x)[metadata]
        metadata[["combine"]] <- TRUE
        metadata <- Filter(Negate(is.null), metadata)

        # Return SingleCellExperiment ------------------------------------------
        args <- list(
            assays = list(counts = counts),
            rowRanges = rowRanges,
            rowData = rowData,
            colData = colData,
            metadata = metadata
        )
        args <- Filter(Negate(is.null), args)
        se <- do.call(what = makeSummarizedExperiment, args = args)
        validObject(se)
        se
    }



.combine.SCE <-  # nolint
    function(x, y) {
        # Coerce to RSE and use combine method.
        Class <- "RangedSummarizedExperiment"  # nolint
        rse <- combine(
            x = as(object = x, Class = Class),
            y = as(object = y, Class = Class)
        )
        validObject(rse)
        # Make SCE from RSE.
        sce <- as(rse, "SingleCellExperiment")
        # FIXME Note that SCE `as()` coercion doesn't currently return valid.
        # FIXME Getting a mismatch between rowData/colData and internal ones.
        # FIXME Add a step to check that sample data will return clean.
        # validObject(sce)
        sce
    }



#' @rdname combine
#' @export
setMethod(
    f = "combine",
    signature = signature(
        x = "SummarizedExperiment",
        y = "SummarizedExperiment"
    ),
    definition = .combine.SE
)



#' @rdname combine
#' @export
setMethod(
    f = "combine",
    signature = signature(
        x = "SingleCellExperiment",
        y = "SingleCellExperiment"
    ),
    definition = .combine.SCE
)
