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
#' @name makeSummarizedExperiment
#' @note Column and rows always return sorted alphabetically.
#' @note Updated 2020-10-01.
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
NULL



## Updated 2020-10-01.
`makeSummarizedExperiment,SimpleList` <- function(  # nolint
    assays,
    rowRanges = GRangesList(),
    rowData = NULL,
    colData = DataFrame(),
    metadata = list(),
    transgeneNames = NULL,
    sort = TRUE,
    sessionInfo = TRUE
) {
    if (!is(assays, "SimpleList")) {
        assays <- SimpleList(assays)
    }
    assert(
        isAny(rowRanges, c("GRanges", "GRangesList", "NULL")),
        isAny(rowData, c("DataFrame", "NULL")),
        isAny(colData, c("DataFrame", "NULL")),
        isAny(metadata, c("list", "NULL")),
        isAny(transgeneNames, c("character", "NULL")),
        isFlag(sort),
        isFlag(sessionInfo)
    )
    ## Only allow `rowData` when `rowRanges` isn't defined.
    if (hasLength(rowRanges)) {
        assert(!hasLength(rowData))
    }

    ## Assays ------------------------------------------------------------------
    ## Drop any `NULL` items in assays.
    assays <- Filter(f = Negate(is.null), x = assays)
    ## Require the primary assay to be named "counts". This helps ensure
    ## consistency with the conventions for `SingleCellExperiment`.
    assert(hasNames(assays), hasValidNames(assays))
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
    ## Dynamically allow input of rowRanges (recommended) or rowData (fallback):
    ## - Detect rows that don't contain annotations.
    ## - Transgenes should contain `transgene` seqname.
    ## - Spike-ins should contain `spike` seqname.
    ## - Non-Ensembl features (e.g. IgG1) will be given `unknown` seqname.
    ## - Error on missing Ensembl features; indicates a version mismatch.
    if (hasLength(rowRanges)) {
        rowData <- NULL
        assert(areIntersectingSets(rownames(assay), names(rowRanges)))
        setdiff <- setdiff(rownames(assay), names(rowRanges))
        mcolnames <- names(mcols(rowRanges))
        if (hasLength(mcolnames)) {
            assert(hasValidNames(mcolnames))
        }
        ## Transgenes.
        if (hasLength(transgeneNames) && hasLength(setdiff)) {
            assert(isSubset(transgeneNames, setdiff))
            transgeneRanges <- emptyRanges(
                names = transgeneNames,
                seqname = "transgene",
                mcolnames = mcolnames
            )
            rowRanges <- suppressWarnings(c(transgeneRanges, rowRanges))
            setdiff <- setdiff(rownames(assay), names(rowRanges))
        }
        ## Additional non-Ensembl gene symbols. Automatically handle extra gene
        ## symbols in 10X Cell Ranger output. For example: CD11b, CD127, HLA-Dr,
        ## IgG1, PD-1, etc.
        pattern <- "^EN[ST].+[0-9.]+$"
        if (
            hasLength(setdiff) &&
            any(grepl(pattern = pattern, x = names(rowRanges)))
        ) {
            symbols <- setdiff[!grepl(pattern = pattern, x = setdiff)]
            cli_alert_warning(sprintf(
                fmt = "%d non-Ensembl %s detected: {.val %s}.",
                length(symbols),
                ngettext(
                    n = length(symbols),
                    msg1 = "feature",
                    msg2 = "features"
                ),
                toString(symbols, width = 200L)
            ))
            cli_alert_info("Define transgenes using {.arg transgeneNames}.")
            unknownRanges <- emptyRanges(
                names = symbols,
                mcolnames = mcolnames
            )
            rowRanges <- suppressWarnings(c(unknownRanges, rowRanges))
            setdiff <- setdiff(rownames(assay), names(rowRanges))
        }
        ## Error on unannotated features. This often indicates an accidental
        ## Ensembl release version mismatch.
        assert(isSubset(rownames(assay), names(rowRanges)))
        rowRanges <- rowRanges[rownames(assay)]
        rowRanges <- encode(rowRanges)
    } else if (hasRows(rowData)) {
        assert(
            isSubset(rownames(assay), rownames(rowData)),
            hasValidNames(rowData)
        )
        rowData <- rowData[rownames(assay), , drop = FALSE]
    }

    ## Column data -------------------------------------------------------------
    if (hasRows(colData)) {
        ## Allowing some single-cell RNA-seq automatic columns to pass through
        ## here, since this code is used by `makeSingleCellExperiment()`. May
        ## tighten this up and be more restrictive in the future.
        blacklist <- setdiff(metadataBlacklist, c("revcomp", "sampleID"))
        assert(
            isSubset(colnames(assay), rownames(colData)),
            areDisjointSets(colnames(colData), blacklist)
        )
        colData <- colData[colnames(assay), , drop = FALSE]
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
    ## Drop factor levels in rowData and colData, to lower memory overhead.
    se <- droplevels(se)
    validObject(se)
    se
}



#' @rdname makeSummarizedExperiment
#' @export
setMethod(
    f = "makeSummarizedExperiment",
    signature = signature(assays = "SimpleList"),
    definition = `makeSummarizedExperiment,SimpleList`
)



## Updated 2019-08-07.
`makeSummarizedExperiment,list` <-  # nolint
    `makeSummarizedExperiment,SimpleList`



#' @rdname makeSummarizedExperiment
#' @export
setMethod(
    f = "makeSummarizedExperiment",
    signature = signature(assays = "list"),
    definition = `makeSummarizedExperiment,list`
)
