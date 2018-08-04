#' Prepare Summarized Experiment
#'
#' This is a utility wrapper for [SummarizedExperiment::SummarizedExperiment()]
#' that provides automatic subsetting for row and column data. It also provides
#' automatic handling of FASTA spike-ins.
#'
#' This function also provides automatic metadata slotting of multiple useful
#' environment parameters:
#'
#' - `date`: Today's date, returned from [base::Sys.Date()].
#' - `wd`: Working directory, returned from [base::getwd()].
#' - `utilsSessionInfo`: [utils::sessionInfo()].
#' - `devtoolsSessionInfo`: [sessioninfo::session_info()].
#'
#' @family Prepare Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param assays `list`. RNA-seq count matrices, which must have matching
#'   dimensions. Counts can be passed in either dense (`matrix`) or sparse
#'   (`dgCMatrix`, `dgTMatrix`) format.
#' @param rowRanges `GRanges` or `NULL`. Metadata describing the
#'   assay rows. If defined, must contain genomic ranges. Can be left `NULL` if
#'   the genome is poorly annotated and/or ranges aren't available from
#'   AnnotationHub.
#' @param colData `DataFrame` `data.frame`, or `matrix`. Metadata
#'   describing the assay columns. For bulk RNA-seq, this data describes the
#'   samples. For single-cell RNA-seq, this data describes the cells.
#' @param metadata `list` or `NULL`. Metadata.
#' @param transgeneNames `character` or `NULL`. Vector indicating which
#'   [assay()] rows denote transgenes (e.g. EGFP, TDTOMATO).
#' @param spikeNames `character` or `NULL`. Vector indicating which [assay()]
#'   rows denote spike-in sequences (e.g. ERCCs).
#'
#' @return `RangedSummarizedExperiment`.
#' @export
#'
#' @seealso
#' - `help("RangedSummarizedExperiment-class", "SummarizedExperiment")`.
#' - `help("SummarizedExperiment-class", "SummarizedExperiment")`.
#' - `help("SingleCellExperiment-class", "SingleCellExperiment")`.
#' - `SummarizedExperiment::SummarizedExperiment()`.
#' - `SingleCellExperiment::SingleCellExperiment()`.
#'
#' @examples
#' genes <- c(
#'     "EGFP",  # transgene
#'     "gene1",
#'     "gene2",
#'     "gene3"
#' )
#' samples <- c(
#'     "sample1",
#'     "sample2",
#'     "sample3",
#'     "sample4"
#' )
#'
#' # Example matrix
#' mat <- matrix(
#'     seq(1L:16L),
#'     nrow = 4L,
#'     ncol = 4L,
#'     dimnames = list(genes, samples)
#' )
#'
#' # Primary assay must be named "counts"
#' assays = list(counts = mat)
#'
#' # rowRanges won't contain transgenes or spike-ins
#' rowRanges <- GRanges(
#'     seqnames = c("1", "1", "1"),
#'     ranges = IRanges(
#'         start = c(1L, 101L, 201L),
#'         end = c(100L, 200L, 300L)
#'     )
#' )
#' names(rowRanges) <- c("gene1", "gene2", "gene3")
#'
#' colData <- data.frame(
#'     genotype = c(
#'         "wildtype",
#'         "wildtype",
#'         "knockout",
#'         "knockout"
#'     ),
#'     age = c(3L, 6L, 3L, 6L),
#'     row.names = samples
#' )
#'
#' prepareSummarizedExperiment(
#'     assays = assays,
#'     rowRanges = rowRanges,
#'     colData = colData,
#'     transgeneNames = "EGFP"
#' )
prepareSummarizedExperiment <- function(
    assays,
    rowRanges = NULL,
    colData = NULL,
    metadata = NULL,
    transgeneNames = NULL,
    spikeNames = NULL
) {
    # Legacy arguments =========================================================
    # nocov start
    call <- match.call()
    if ("isSpike" %in% names(call)) {
        warning("Use `spikeNames` instead of `isSpike`")
        spikeNames <- call[["isSpike"]]
    }
    # nocov end

    # Assert checks ============================================================
    assert_is_any_of(assays, c("list", "ShallowSimpleListAssays", "SimpleList"))
    assert_is_any_of(rowRanges, c("GRanges", "NULL"))
    assert_is_any_of(colData, c("DataFrame", "data.frame", "NULL"))
    assert_is_any_of(metadata, c("list", "NULL"))
    assert_is_any_of(transgeneNames, c("character", "NULL"))
    assert_is_any_of(spikeNames, c("character", "NULL"))

    # Assays ===================================================================
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

    # Row ranges ===============================================================
    # Detect rows that don't contain annotations.
    # Transgenes should contain `transgene` seqname
    # Spike-ins should contain `spike` seqname
    # Otherwise, unannotated genes will be given `unknown` seqname
    setdiff <- setdiff(rownames(assay), names(rowRanges))

    if (is(rowRanges, "GRanges")) {
        assert_are_intersecting_sets(rownames(assay), names(rowRanges))
        mcolsNames <- names(mcols(rowRanges))

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
    } else {
        rowRanges <- GRanges()
        mcolsNames <- NULL
    }

    # Label any unannotated genes with `unknown` seqname
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

    # Sort the rowRanges to match assay
    assert_is_subset(rownames(assay), names(rowRanges))
    rowRanges <- rowRanges[rownames(assay)]

    # Column data ==============================================================
    if (is.null(colData)) {
        colData <- DataFrame(row.names = colnames(assay))
    } else {
        assert_are_identical(colnames(assay), rownames(colData))
        colData <- as(colData, "DataFrame")
    }

    # Metadata =================================================================
    metadata <- as.list(metadata)
    metadata[["date"]] <- Sys.Date()
    metadata[["wd"]] <- normalizePath(".", winslash = "/", mustWork = TRUE)
    metadata[["utilsSessionInfo"]] <- sessionInfo()
    metadata[["devtoolsSessionInfo"]] <- session_info(include_base = TRUE)
    metadata <- Filter(Negate(is.null), metadata)

    # Return ===================================================================
    SummarizedExperiment(
        assays = assays,
        rowRanges = rowRanges,
        colData = colData,
        metadata = metadata
    )
}
