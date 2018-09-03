#' Make Summarized Experiment
#'
#' This function is a utility wrapper for
#' [SummarizedExperiment::SummarizedExperiment()] that provides automatic
#' subsetting for row and column data, as well as automatic handling of
#' transgenes and spike-ins. Additionally, it improves upon the standard
#' constructor by slotting useful session information into the [metadata()]
#' slot:
#'
#' - `date`: Today's date, returned from [base::Sys.Date()].
#' - `wd`: Working directory, returned from [base::getwd()].
#' - `utilsSessionInfo`: [utils::sessionInfo()].
#' - `devtoolsSessionInfo`: [sessioninfo::session_info()].
#'
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param assays `list`. RNA-seq count matrices, which must have matching
#'   dimensions. Counts can be passed in either dense (`matrix`) or sparse
#'   (`dgCMatrix`, `dgTMatrix`) format.
#' @param rowRanges `GRanges` or `NULL`. *Recommended*. Metadata describing the
#'   assay rows. If defined, must contain genomic ranges. Can be left `NULL` if
#'   the genome is poorly annotated and/or ranges aren't available from
#'   AnnotationHub.
#' @param rowData `DataFrame` or `NULL`. *Not recommended*. Metadata describing
#'   the assay rows, if genomic ranges are not available.
#' @param colData `DataFrame` `data.frame`, or `matrix`. Metadata
#'   describing the assay columns. For bulk RNA-seq, this data describes the
#'   samples. For single-cell RNA-seq, this data describes the cells.
#' @param metadata `list` or `NULL`. Metadata.
#' @param transgeneNames `character` or `NULL`. Vector indicating which
#'   [assay()] rows denote transgenes (e.g. EGFP, TDTOMATO).
#' @param spikeNames `character` or `NULL`. Vector indicating which [assay()]
#'   rows denote spike-in sequences (e.g. ERCCs).
#'
#' @return
#' - Providing `rowRanges`: `RangedSummarizedExperiment`.
#' - Providing `rowData`: `SummarizedExperiment`.
#'
#' @seealso
#' - `help("RangedSummarizedExperiment-class", "SummarizedExperiment")`.
#' - `help("SummarizedExperiment-class", "SummarizedExperiment")`.
#' - `help("SingleCellExperiment-class", "SingleCellExperiment")`.
#' - `SummarizedExperiment::SummarizedExperiment()`.
#' - `SingleCellExperiment::SingleCellExperiment()`.
#'
#' @examples
#' # Rows (genes)
#' genes <- c(
#'     "EGFP",  # transgene (see below)
#'     paste0("gene", seq_len(3L))
#' )
#' print(genes)
#'
#' # Columns (samples)
#' samples <- paste0("sample", seq_len(4L))
#' print(samples)
#'
#' # Counts (assay)
#' counts <- matrix(
#'     data = seq_len(16L),
#'     nrow = 4L,
#'     ncol = 4L,
#'     dimnames = list(genes, samples)
#' )
#' # Primary assay must be named "counts".
#' assays <- list(counts = counts)
#' print(assays)
#'
#' # Row data (genomic ranges)
#' rowRanges <- GRanges(
#'     seqnames = rep("1", times = 3L),
#'     ranges = IRanges(
#'         start = seq(from = 1L, to = 201L, by = 100L),
#'         end = seq(from = 100L, to = 300L, by = 100L)
#'     )
#' )
#' # Note that we haven't defined the transgene here.
#' # It will be handled automatically in the function call.
#' names(rowRanges) <- paste0("gene", seq_len(3L))
#' print(rowRanges)
#'
#' # Column data
#' colData <- DataFrame(
#'     genotype = rep(c("wildtype", "knockout"), times = 1L, each = 2L),
#'     age = rep(c(3L, 6L), 2L),
#'     row.names = samples
#' )
#' print(colData)
#'
#' makeSummarizedExperiment(
#'     assays = assays,
#'     rowRanges = rowRanges,
#'     colData = colData,
#'     transgeneNames = "EGFP"
#' )
makeSummarizedExperiment <- function(
    assays,
    rowRanges = NULL,
    rowData = NULL,
    colData = NULL,
    metadata = NULL,
    transgeneNames = NULL,
    spikeNames = NULL
) {
    # Assert checks ------------------------------------------------------------
    assert_is_any_of(assays, c("list", "ShallowSimpleListAssays", "SimpleList"))
    assert_is_any_of(rowRanges, c("GRanges", "NULL"))
    assert_is_any_of(rowData, c("DataFrame", "NULL"))
    # Only allow `rowData` if `rowRanges` are `NULL`.
    if (!is.null(rowRanges)) {
        assert_is_null(rowData)
    }
    assert_is_any_of(colData, c("DataFrame", "NULL"))
    assert_is_any_of(metadata, c("list", "NULL"))
    assert_is_any_of(transgeneNames, c("character", "NULL"))
    assert_is_any_of(spikeNames, c("character", "NULL"))

    # Assays -------------------------------------------------------------------
    # Require the primary assay matrix to be named counts. This helps ensure
    # consistency with the conventions for SingleCellExperiment.
    assert_are_identical(names(assays)[[1L]], "counts")
    if (is.list(assays)) {
        assays <- Filter(Negate(is.null), assays)
    }
    assay <- assays[[1L]]
    assert_has_dimnames(assay)
    assert_has_rownames(assay)
    assert_has_colnames(assay)
    assert_has_no_duplicates(rownames(assay))
    assert_has_no_duplicates(colnames(assay))
    assert_are_identical(
        x = makeNames(rownames(assay), unique = TRUE),
        y = rownames(assay)
    )
    assert_are_identical(
        x = makeNames(colnames(assay), unique = TRUE),
        y = colnames(assay)
    )

    # Row data -----------------------------------------------------------------
    mcolsNames <- NULL
    # Dynamically allow input of rowRanges (recommended) or rowData (fallback).
    if (is(rowRanges, "GRanges")) {
        # Detect rows that don't contain annotations.
        # Transgenes should contain `transgene` seqname.
        # Spike-ins should contain `spike` seqname.
        # Otherwise, unannotated genes will be given `unknown` seqname.
        assert_are_intersecting_sets(rownames(assay), names(rowRanges))
        mcolsNames <- names(mcols(rowRanges))
        setdiff <- setdiff(rownames(assay), names(rowRanges))

        # Transgenes
        if (length(setdiff) && length(transgeneNames)) {
            assert_is_subset(transgeneNames, setdiff)
            transgeneRanges <- emptyRanges(
                names = transgeneNames,
                seqname = "transgene",
                mcolsNames = mcolsNames
            )
            rowRanges <- suppressWarnings(c(transgeneRanges, rowRanges))
            setdiff <- setdiff(rownames(assay), names(rowRanges))
        }

        # FASTA spike-ins
        if (length(setdiff) && length(spikeNames)) {
            assert_is_subset(spikeNames, setdiff)
            spikeRanges <- emptyRanges(
                names = spikeNames,
                seqname = "spike",
                mcolsNames = mcolsNames
            )
            rowRanges <- suppressWarnings(c(spikeRanges, rowRanges))
            setdiff <- setdiff(rownames(assay), names(rowRanges))
        }
    } else if (is(rowData, "DataFrame")) {
        warning("`rowRanges` usage is recommended over `rowData`")
        assert_is_subset(rownames(assay), rownames(rowData))
        rowData <- rowData[rownames(assay), , drop = FALSE]
    } else {
        # Fallback support when working with poorly annotation genomes.
        rowRanges <- GRanges()
    }

    # Final processing of rowRanges, if defined.
    # Automatically slot empty ranges, if necessary, and inform the user.
    if (is(rowRanges, "GRanges")) {
        # Label any remaining unannotated genes with `unknown` seqname.
        if (length(setdiff)) {
            warning(paste(
                paste(
                    "Unannotated rows detected",
                    paste0("(", length(setdiff), "):")
                ),
                str_trunc(toString(setdiff), width = 80L),
                sep = "\n"
            ))
            unknownRanges <- emptyRanges(
                names = setdiff,
                seqname = "unknown",
                mcolsNames = mcolsNames
            )
            rowRanges <- suppressWarnings(c(unknownRanges, rowRanges))
        }
        # Sort the rowRanges to match assay.
        assert_is_subset(rownames(assay), names(rowRanges))
        rowRanges <- rowRanges[rownames(assay)]
    }

    # Column data --------------------------------------------------------------
    if (is.null(colData)) {
        warning("`colData` is recommended")
        colData <- DataFrame(row.names = colnames(assay))
    }
    assert_is_subset(colnames(assay), rownames(colData))
    colData <- colData[colnames(assay), , drop = FALSE]

    # Metadata -----------------------------------------------------------------
    metadata <- as.list(metadata)
    metadata[["date"]] <- Sys.Date()
    metadata[["wd"]] <- normalizePath(".", winslash = "/", mustWork = TRUE)
    metadata[["utilsSessionInfo"]] <- sessionInfo()
    metadata[["devtoolsSessionInfo"]] <- session_info(include_base = TRUE)
    metadata <- Filter(Negate(is.null), metadata)

    # Return -------------------------------------------------------------------
    args <- list(
        assays = assays,
        rowRanges = rowRanges,
        rowData = rowData,
        colData = colData,
        metadata = metadata
    )
    args <- Filter(Negate(is.null), args)
    do.call(
        what = SummarizedExperiment,
        args = args
    )
}
