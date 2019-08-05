## FIXME Check for valid names in all slots. rownames, colnames, colData, rowData, metadata



#' Make a SummarizedExperiment object
#'
#' This function is a utility wrapper for `SummarizedExperiment` that provides
#' automatic subsetting for row and column data, as well as automatic handling
#' of transgenes and spike-ins.
#'
#' @section Session information:
#'
#' This function improves upon the standard constructor by slotting useful
#' session information into the `metadata` slot by default:
#'
#' - `date`: Today's date, returned from `Sys.Date`.
#' - `wd`: Working directory, returned from `getwd`.
#' - `sessionInfo`: [sessioninfo::session_info()] return.
#'
#' This behavior can be disabled by setting `sessionInfo = FALSE`.
#'
#' @note Column and rows always return sorted alphabetically.
#' @note Updated 2019-08-01.
#' @export
#'
#' @inheritParams acidroxygen::params
#' @param sort `logical(1)`.
#'   Ensure all row and column names are sorted alphabetically. This includes
#'   columns inside `rowData` and `colData`, and `metadata` slot names. Assay
#'   names are required to contain `counts` as the first assay.
#' @param sessionInfo `logical(1)`.
#'   Slot session information into `metadata`.
#'
#' @return
#' - Providing `rowRanges`: `RangedSummarizedExperiment`.
#' - Providing `rowData`: `SummarizedExperiment`.
#'
#' @seealso
#' - [`SummarizedExperiment()`][SummarizedExperiment::SummarizedExperiment].
#' - [`SingleCellExperiment()`][SingleCellExperiment::SingleCellExperiment].
#' - `help("RangedSummarizedExperiment-class", "SummarizedExperiment")`.
#' - `help("SummarizedExperiment-class", "SummarizedExperiment")`.
#' - `help("SingleCellExperiment-class", "SingleCellExperiment")`.
#'
#' @examples
#' ## Rows (genes)
#' genes <- c(
#'     sprintf("gene%02d", seq_len(3L)),
#'     "EGFP"  # transgene
#' )
#' print(genes)
#'
#' ## Columns (samples)
#' samples <- sprintf("sample%02d", seq_len(4L))
#' print(samples)
#'
#' ## Counts (assay)
#' counts <- matrix(
#'     data = seq_len(length(genes) * length(samples)),
#'     nrow = length(genes),
#'     ncol = length(samples),
#'     dimnames = list(genes, samples)
#' )
#' ## Primary assay must be named "counts".
#' assays <- SimpleList(counts = counts)
#' print(assays)
#'
#' ## Row data (genomic ranges)
#' ## Note that we haven't defined the transgene here.
#' ## It will be handled automatically in the function call.
#' rowRanges <- emptyRanges(names = head(genes, n = length(genes) - 1L))
#' print(rowRanges)
#'
#' ## Column data
#' colData <- DataFrame(
#'     age = rep(
#'         x = c(3L, 6L),
#'         times = length(samples) / 2L
#'     ),
#'     genotype = rep(
#'         x = c("wildtype", "knockout"),
#'         times = 1L,
#'         each = length(samples) / 2L
#'     ),
#'     row.names = samples
#' )
#' print(colData)
#'
#' ## Minimal mode.
#' x <- makeSummarizedExperiment(assays = assays)
#' print(x)
#'
#' x <- makeSummarizedExperiment(
#'     assays = assays,
#'     rowRanges = rowRanges,
#'     colData = colData,
#'     transgeneNames = "EGFP"
#' )
#' print(x)
makeSummarizedExperiment <- function(
    assays,
    rowRanges,
    rowData,
    colData,
    metadata,
    transgeneNames = NULL,
    spikeNames = NULL,
    sort = TRUE,
    sessionInfo = TRUE
) {
    assert(
        isAny(
            x = assays,
            classes = c("SimpleList", "list")
        ),
        isAny(
            x = rowRanges,
            classes = c("GRanges", "GRangesList", "NULL")
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
        ),
        isFlag(sort),
        isFlag(sessionInfo)
    )

    ## Only allow `rowData` if `rowRanges` are not defined.
    if (hasLength(rowRanges)) {
        assert(!hasLength(rowData))
    }

    ## Assays ------------------------------------------------------------------
    ## Coerce to `list` class to `SimpleList`, for consistency.
    if (is.list(assays)) {
        assays <- as(assays, "SimpleList")
    }

    ## Drop any `NULL` items in assays.
    assays <- Filter(f = Negate(is.null), x = assays)
    assert(is(assays, "SimpleList"))

    ## Require the primary assay to be named "counts". This helps ensure
    ## consistency with the conventions for `SingleCellExperiment`.
    assert(
        hasNames(assays),
        hasValidNames(assays),
        identical(names(assays)[[1L]], "counts")
    )
    assay <- assays[[1L]]

    ## Require valid names for both columns (samples) and rows (genes). Note
    ## that values beginning with a number or containing invalid characters
    ## (e.g. spaces, dashes) will error here. This assert will pass if the
    ## assay does not contain any row or column names.
    assert(hasValidDimnames(assay))

    ## We're going to sort the assay names by default, but will perform this
    ## step after generating the `SummarizedExperiment` object (see below). The
    ## `SummarizedExperiment()` constructor checks to ensure that all assays
    ## have matching dimnames, so we can skip that check.

    ## Row data ----------------------------------------------------------------
    ## Dynamically allow input of rowRanges (recommended) or rowData (fallback).
    if (hasLength(rowRanges)) {
        ## Detect rows that don't contain annotations.
        ## Transgenes should contain `transgene` seqname.
        ## Spike-ins should contain `spike` seqname.
        ## Otherwise, unannotated genes will be given `unknown` seqname.
        assert(areIntersectingSets(rownames(assay), names(rowRanges)))
        mcolnames <- names(mcols(rowRanges))
        setdiff <- setdiff(rownames(assay), names(rowRanges))
        ## Transgenes
        if (hasLength(transgeneNames) && hasLength(setdiff)) {
            assert(isSubset(x = transgeneNames, y = setdiff))
            transgeneRanges <- emptyRanges(
                names = transgeneNames,
                seqname = "transgene",
                mcolnames = mcolnames
            )
            rowRanges <- suppressWarnings(c(transgeneRanges, rowRanges))
            setdiff <- setdiff(rownames(assay), names(rowRanges))
        }
        ## FASTA spike-ins
        if (hasLength(spikeNames) && hasLength(setdiff)) {
            assert(isSubset(x = spikeNames, y = setdiff))
            spikeRanges <- emptyRanges(
                names = spikeNames,
                seqname = "spike",
                mcolnames = mcolnames
            )
            rowRanges <- suppressWarnings(c(spikeRanges, rowRanges))
            setdiff <- setdiff(rownames(assay), names(rowRanges))
        }
        ## Automatically arrange the ranges to match the main assay.
        assert(isSubset(rownames(assay), names(rowRanges)))
        rowRanges <- rowRanges[rownames(assay)]
        ## Releving here to avoid factor levels blowing up the object size.
        rowRanges <- relevel(rowRanges)
        if (hasCols(mcols(rowRanges))) {
            assert(hasValidNames(mcols(rowRanges)))
        }
    } else if (hasRows(rowData)) {
        ## Automatically arrange the rows to match the main assay.
        assert(isSubset(rownames(assay), rownames(rowData)))
        rowData <- rowData[rownames(assay), , drop = FALSE]
        rowData <- relevel(rowData)
        if (hasCols(rowData)) {
            assert(hasValidNames(rowData))
        }
    }

    ## Error on user-defined row annotation mismatch. This step is important, to
    ## catch users accidentially attempting to use the wrong version of genome
    ## annotations.
    if (hasRownames(assay)) {
        if (hasLength(rowRanges)) {
            data <- as(rowRanges, "DataFrame")
        } else if (hasRows(rowData)) {
            data <- rowData
        } else {
            data <- NULL
        }
        if (hasRows(data)) {
            setdiff <- setdiff(rownames(assay), rownames(data))
            if (hasLength(setdiff)) {
                stop(sprintf(
                    fmt = paste0(
                        "%d unannotated rows: %s\n",
                        "Check genome build and release version.\n",
                        "Define transgenes with `transgeneNames`",
                        "and spike-ins with `spikeNames`.",
                    ),
                    length(setdiff),
                    toString(setdiff, width = 200L)
                ))
            }
        }
        rm(data)
    }

    ## Column data -------------------------------------------------------------
    if (hasRows(colData)) {
        ## Allowing some single-cell RNA-seq automatic columns to pass through
        ## here, since this code is used by `makeSingleCellExperiment()`. May
        ## tighten this up and be more restrictive in the future.
        blacklist <- setdiff(
            x = metadataBlacklist,
            y = c("revcomp", "sampleID")
        )
        assert(
            isSubset(colnames(assay), rownames(colData)),
            areDisjointSets(colnames(colData), blacklist)
        )
        colData <- colData[colnames(assay), , drop = FALSE]
        colData <- relevel(colData)
    }
    if (hasCols(colData)) {
        assert(hasValidNames(colData))
    }

    ## Metadata ----------------------------------------------------------------
    if (is.null(metadata)) {
        metadata <- list()
    }
    if (isTRUE(sessionInfo)) {
        metadata[["date"]] <- Sys.Date()
        metadata[["sessionInfo"]] <- session_info(include_base = TRUE)
        metadata[["wd"]] <- realpath(".")
    }
    metadata <- Filter(f = Negate(is.null), x = metadata)
    if (hasLength(metadata)) {
        assert(hasValidNames(metadata))
    }

    ## Return ------------------------------------------------------------------
    ## Ensure we're not passing any `NULL` or empty arguments to
    ## `SummarizedExperiment` generator function. This step will dynamically
    ## handle `rowRanges` and/or `rowData`.
    args <- list(
        assays = assays,
        rowRanges = rowRanges,
        rowData = rowData,
        colData = colData,
        metadata = metadata
    )
    args <- Filter(f = Negate(is.null), x = args)
    args <- Filter(f = hasLength, x = args)
    se <- do.call(what = SummarizedExperiment, args = args)

    if (isTRUE(sort)) {
        ## Assay names. Always keep counts first.
        assayNames <- assayNames(se)
        if (length(assayNames) > 1L) {
            assayNames <- unique(c("counts", sort(assayNames)))
            assays(se) <- assays(se)[assayNames]
        }

        ## Assay rows and columns.
        se <- se[sort(rownames(se)), sort(colnames(se))]

        ## Row data columns.
        rowData(se) <- rowData(se)[, sort(colnames(rowData(se))), drop = FALSE]

        ## Column data columns.
        colData(se) <- colData(se)[, sort(colnames(colData(se))), drop = FALSE]

        ## Metadata names.
        metadata(se) <- metadata(se)[sort(names(metadata(se)))]
    }

    assert(identical(assayNames(se)[[1L]], "counts"))
    validObject(se)
    se
}

## Update formals to match current `SimpleList` S4 method.
args <- c("colData", "metadata", "rowData", "rowRanges")
f <- methodFormals(
    f = "SummarizedExperiment",
    signature = signature(assays = "SimpleList"),
    package = "SummarizedExperiment"
)
formals(makeSummarizedExperiment)[args] <- f[args]
