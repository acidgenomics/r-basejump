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
#' @param metadata `character`. Metadata slot names that must be identical
#'   between the datasets.
#'
#' @seealso [BiocGenerics::combine()].
#'
#' @return `SummarizedExperiment`.
#'
#' @examples
#' # For this simple working example, let's duplicate our minimal dataset.
#'
#' x <- rse_small
#' colnames(x)
#' colData(x)
#'
#' y <- x
#' colnames(y) <- paste0("sample", seq(from = 5L, to = 8L))
#' colnames(y)
#' colData(y)[["sampleName"]] <- colnames(y)
#' colData(y)
#'
#' # Combine two SummarizedExperiment objects.
#' c <- combine(x, y)
#' print(c)
#' colnames(c)
#' colData(c)
NULL



.combine.SE <-  # nolint
    function(
        x,
        y,
        metadata = c(
            "version",
            "interestingGroups",
            "organism",
            "genomeBuild",
            "ensemblRelease",
            "rowRangesMetadata",
            "gffFile"
        )
    ) {
        assert_is_character(metadata)

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

        # Require that specific metadata is identical.
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
        do.call(what = makeSummarizedExperiment, args = args)
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
