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
#' @note Column and rows always return sorted alphabetically.
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
    rowRanges = NULL,  # recommended
    rowData = NULL,    # not recommended
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
    # FIXME Simplify the assert checks here?
    assert_has_dimnames(assay)
    assert_has_rownames(assay)
    assert_has_colnames(assay)
    assert_has_no_duplicates(rownames(assay))
    assert_has_no_duplicates(colnames(assay))
    # Columns and rows must contain valid names.
    # FIXME Switch to using an S4 method on matrix here.
    assert_are_identical(
        x = makeNames(rownames(assay), unique = TRUE),
        y = rownames(assay)
    )
    assert_are_identical(
        x = makeNames(colnames(assay), unique = TRUE),
        y = colnames(assay)
    )

    # Require that assay row and column names are sorted.

    # Row data -----------------------------------------------------------------
    mcolsNames <- NULL
    # Dynamically allow input of rowRanges (recommended) or rowData (fallback).
    if (is(rowRanges, "GRanges")) {
        userRows <- TRUE
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
        userRows <- TRUE
        assert_is_subset(rownames(assay), rownames(rowData))
        rowData <- rowData[rownames(assay), , drop = FALSE]
    } else {
        # Fallback support when working with poorly annotation genomes.
        userRows <- FALSE
        rowRanges <- GRanges()
    }

    # Check for unannotated rows and inform the user.
    if (is(rowRanges, "GRanges")) {
        data <- as(rowRanges, "DataFrame")
    } else if (is(rowData, "DataFrame")) {
        data <- rowData
    } else {
        data <- NULL
    }
    if (!is.null(data)) {
        setdiff <- setdiff(rownames(assay), rownames(data))
        if (length(setdiff)) {
            # If user has provided the row annotations, inform them when some
            # rows are unannotated.
            if (isTRUE(userRows)) {
                # Stop on too may unannotated rows, otherwise warn.
                # This should cover usage of old built-in bcbio genomes.
                if (length(setdiff) > 500L) {
                    f <- stop
                } else {
                    f <- warning
                }
                f(paste(
                    # 24 characters (see trunc call below).
                    paste(
                        "Unannotated rows",
                        paste0("(", length(setdiff), "):"),
                        str_trunc(
                            string = toString(setdiff),
                            width = getOption("width") - 24L
                        )
                    ),
                    "Consider regenerating the SummarizedExperiment.",
                    "Check that your genome build and release are correct.",
                    "Consider using a GTF/GFF file for row annotations.",
                    paste(
                        "Define transgenes with `transgeneNames`",
                        "and spike-ins with `spikeNames`."
                    ),
                    sep = "\n"
                ))
            }
            # Define the unknown ranges in rowRanges, if necessary.
            if (is(rowRanges, "GRanges")) {
                unknownRanges <- emptyRanges(
                    names = setdiff,
                    seqname = "unknown",
                    mcolsNames = mcolsNames
                )
                rowRanges <- suppressWarnings(c(unknownRanges, rowRanges))
            }
        }
    }
    rm(data)

    # Automatically arrange the rows to match the main assay.
    if (is(rowRanges, "GRanges")) {
        assert_has_names(rowRanges)
        assert_is_subset(rownames(assay), names(rowRanges))
        rowRanges <- rowRanges[rownames(assay)]
    } else if (is(rowData, "DataFrame")) {
        assertHasRownames(rowData)
        assert_is_subset(rownames(assay), rownames(rowData))
        rowData <- rowData[rownames(assay), , drop = FALSE]
    }

    # Column data --------------------------------------------------------------
    if (is.null(colData)) {
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
