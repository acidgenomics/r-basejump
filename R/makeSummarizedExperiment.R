#' Make a `SummarizedExperiment` object
#'
#' This function is a utility wrapper for `SummarizedExperiment` that provides
#' automatic subsetting for row and column data, as well as automatic handling
#' of transgenes and spike-ins. Additionally, it improves upon the standard
#' constructor by slotting useful session information into the `metadata` slot:
#'
#' - `date`: Today's date, returned from `Sys.Date`.
#' - `wd`: Working directory, returned from `getwd`.
#' - `sessionInfo`: [sessioninfo::session_info()] return.
#'
#' @note Column and rows always return sorted alphabetically.
#'
#' @export
#'
#' @inheritParams params
#'
#' @return
#' - Providing `rowRanges`: `RangedSummarizedExperiment`.
#' - Providing `rowData`: `SummarizedExperiment`.
#'
#' @seealso
#' - `SummarizedExperiment::SummarizedExperiment()`.
#' - `SingleCellExperiment::SingleCellExperiment()`.
#' - `help("RangedSummarizedExperiment-class", "SummarizedExperiment")`.
#' - `help("SummarizedExperiment-class", "SummarizedExperiment")`.
#' - `help("SingleCellExperiment-class", "SingleCellExperiment")`.
#'
#' @examples
#' library(IRanges)
#' library(GenomicRanges)
#' library(SummarizedExperiment)
#'
#' ## Rows (genes)
#' genes <- c(
#'     "EGFP",  # transgene
#'     paste0("gene", seq_len(3L))
#' )
#' print(genes)
#'
#' ## Columns (samples)
#' samples <- paste0("sample", seq_len(4L))
#' print(samples)
#'
#' ## Counts (assay)
#' counts <- matrix(
#'     data = seq_len(16L),
#'     nrow = 4L,
#'     ncol = 4L,
#'     dimnames = list(genes, samples)
#' )
#' ## Primary assay must be named "counts".
#' assays <- list(counts = counts)
#' print(assays)
#'
#' ## Row data (genomic ranges)
#' rowRanges <- GRanges(
#'     seqnames = rep("1", times = 3L),
#'     ranges = IRanges(
#'         start = seq(from = 1L, to = 201L, by = 100L),
#'         end = seq(from = 100L, to = 300L, by = 100L)
#'     )
#' )
#' ## Note that we haven't defined the transgene here.
#' ## It will be handled automatically in the function call.
#' names(rowRanges) <- paste0("gene", seq_len(3L))
#' print(rowRanges)
#'
#' ## Column data
#' colData <- S4Vectors::DataFrame(
#'     genotype = rep(c("wildtype", "knockout"), times = 1L, each = 2L),
#'     age = rep(c(3L, 6L), 2L),
#'     row.names = samples
#' )
#' print(colData)
#'
#' ## Note that this returns with the dimnames sorted.
#' x <- makeSummarizedExperiment(
#'     assays = assays,
#'     rowRanges = rowRanges,
#'     colData = colData,
#'     transgeneNames = "EGFP"
#' )
#' print(x)
makeSummarizedExperiment <- function(
    assays,
    rowRanges = NULL,  # recommended
    rowData = NULL,    # legacy
    colData = NULL,
    metadata = NULL,
    transgeneNames = NULL,
    spikeNames = NULL
) {
    # Assert checks ------------------------------------------------------------
    assert(
        isAny(
            x = assays,
            classes = c("list", "ShallowSimpleListAssays", "SimpleList")
        ),
        isAny(
            x = rowRanges,
            classes = c("GRanges", "NULL")
        ),
        isAny(
            x = rowData,
            classes = c("DataFrame", "NULL")
        ),
        isAny(
            x = colData,
            classes = c("DataFrame", "NULL")
        ),
        isAny(
            x = metadata,
            classes = c("list", "NULL")
        ),
        isAny(
            x = transgeneNames,
            classes = c("character", "NULL")
        ),
        isAny(
            x = spikeNames,
            classes = c("character", "NULL")
        )
    )

    # Only allow `rowData` when `rowRanges` are `NULL`.
    if (!is.null(rowRanges)) {
        assert(is.null(rowData))
    }

    # Assays -------------------------------------------------------------------
    # Drop any `NULL` items in assays.
    if (is.list(assays)) {
        assays <- Filter(Negate(is.null), assays)
    }
    # Require the primary assay to be named "counts". This helps ensure
    # consistency with the conventions for `SingleCellExperiment`.
    assert(identical(names(assays)[[1L]], "counts"))
    assay <- assays[[1L]]
    # Require valid names for both columns (samples) and rows (genes).
    # Note that values beginning with a number or containing invalid characters
    # (e.g. spaces, dashes) will error here.
    assert(hasValidDimnames(assay))
    # We're going to require that the assay names be sorted, but will perform
    # this step after generating the `SummarizedExperiment` object (see below).
    # The `SummarizedExperiment` constructor checks to ensure that all assays
    # have matching dimnames, so we can skip that check.

    # Row data -----------------------------------------------------------------
    mcolsNames <- NULL
    # Dynamically allow input of rowRanges (recommended) or rowData (fallback).
    if (is(rowRanges, "GRanges")) {
        # Detect rows that don't contain annotations.
        # Transgenes should contain `transgene` seqname.
        # Spike-ins should contain `spike` seqname.
        # Otherwise, unannotated genes will be given `unknown` seqname.
        assert(areIntersectingSets(rownames(assay), names(rowRanges)))
        mcolsNames <- names(mcols(rowRanges))
        setdiff <- setdiff(rownames(assay), names(rowRanges))
        # Transgenes
        if (
            length(setdiff) > 0L &&
            length(transgeneNames) > 0L
        ) {
            assert(isSubset(x = transgeneNames, y = setdiff))
            transgeneRanges <- emptyRanges(
                names = transgeneNames,
                seqname = "transgene",
                mcolsNames = mcolsNames
            )
            rowRanges <- suppressWarnings(c(transgeneRanges, rowRanges))
            setdiff <- setdiff(rownames(assay), names(rowRanges))
        }
        # FASTA spike-ins
        if (
            length(setdiff) > 0L &&
            length(spikeNames) > 0L
        ) {
            assert(isSubset(x = spikeNames, y = setdiff))
            spikeRanges <- emptyRanges(
                names = spikeNames,
                seqname = "spike",
                mcolsNames = mcolsNames
            )
            rowRanges <- suppressWarnings(c(spikeRanges, rowRanges))
            setdiff <- setdiff(rownames(assay), names(rowRanges))
        }
    } else if (is(rowData, "DataFrame")) {
        assert(isSubset(rownames(assay), rownames(rowData)))
        rowData <- rowData[rownames(assay), , drop = FALSE]
    } else {
        message("Slotting empty ranges.")
        rowRanges <- emptyRanges(names = rownames(assay))
    }

    # Error on user-defined row annotation mismatch (strict).
    if (is(rowRanges, "GRanges")) {
        data <- as(rowRanges, "DataFrame")
    } else if (is(rowData, "DataFrame")) {
        data <- rowData
    }
    assert(is(data, "DataFrame"))
    setdiff <- setdiff(rownames(assay), rownames(data))
    if (length(setdiff) > 0L) {
        stop(paste0(
            "Unannotated rows (", length(setdiff), "): ",
            str_trunc(
                string = toString(setdiff),
                width = getOption("width") - 24L
            ), "\n",
            "Check that your genome build and Ensembl release are correct.\n",
            "Consider using a GTF/GFF file.\n",
            "Define transgenes with `transgeneNames`",
            "and spike-ins with `spikeNames`."
        ))
    }
    rm(data)

    # Automatically arrange the rows to match the main assay.
    if (is(rowRanges, "GRanges")) {
        assert(hasNames(rowRanges))
        assert(isSubset(rownames(assay), names(rowRanges)))
        rowRanges <- rowRanges[rownames(assay)]
    } else if (is(rowData, "DataFrame")) {
        assert(hasRownames(rowData))
        assert(isSubset(rownames(assay), rownames(rowData)))
        rowData <- rowData[rownames(assay), , drop = FALSE]
    }

    # Column data --------------------------------------------------------------
    if (is.null(colData)) {
        colData <- DataFrame(row.names = colnames(assay))
    }
    assert(isSubset(colnames(assay), rownames(colData)))
    colData <- colData[colnames(assay), , drop = FALSE]

    # Metadata -----------------------------------------------------------------
    metadata <- as.list(metadata)
    metadata[["date"]] <- Sys.Date()
    metadata[["wd"]] <- realpath(".")
    metadata[["sessionInfo"]] <- session_info(include_base = TRUE)
    metadata <- Filter(f = Negate(is.null), x = metadata)

    # Return -------------------------------------------------------------------
    args <- list(
        assays = assays,
        rowRanges = rowRanges,
        rowData = rowData,
        colData = colData,
        metadata = metadata
    )
    # Ensure we're not passing any `NULL` arguments to `do.call`.
    # This step will dynamically handle `rowRanges` and/or `rowData`.
    args <- Filter(Negate(is.null), args)
    se <- do.call(what = SummarizedExperiment, args = args)
    # Always return with sorted rows and columns.
    se <- se[sort(rownames(se)), sort(colnames(se))]
    validObject(se)
    se
}
