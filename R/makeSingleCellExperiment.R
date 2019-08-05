#' Make a SingleCellExperiment object
#'
#' @include makeSummarizedExperiment.R
#' @inherit makeSummarizedExperiment
#' @note Updated 2019-08-01.
#' @export
#'
#' @inheritParams acidroxygen::params
#'
#' @return `SingleCellExperiment`.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#'
#' object <- SingleCellExperiment
#' assays <- assays(object)
#' rowRanges <- rowRanges(object)
#' colData <- colData(object)
#' metadata <- metadata(object)
#' reducedDims <- reducedDims(object)
#'
#' x <- makeSingleCellExperiment(
#'     assays = assays,
#'     rowRanges = rowRanges,
#'     colData = colData,
#'     metadata = metadata,
#'     reducedDims = reducedDims
#' )
#' print(x)
makeSingleCellExperiment <- function(
    assays,
    rowRanges,
    colData,
    metadata,
    reducedDims,
    transgeneNames = NULL,
    spikeNames = NULL
) {
    assert(
        isAny(
            x = reducedDims,
            classes = c("SimpleList", "list", "NULL")
        )
    )

    ## Coerce reducedDims to SimpleList, for consistency.
    if (is.list(reducedDims)) {
        reducedDims <- as(reducedDims, "SimpleList")
    } else if (is.null(reducedDims)) {
        reducedDims <- SimpleList()
    }
    if (hasLength(reducedDims)) {
        assert(hasValidNames(reducedDims))
    }

    ## Make SummarizedExperiment first.
    ## Supports automatic resizing of rowRanges and helps slot FASTA spike-ins.
    se <- makeSummarizedExperiment(
        assays = assays,
        rowRanges = rowRanges,
        colData = colData,
        metadata = metadata,
        transgeneNames = transgeneNames,
        spikeNames = spikeNames
    )

    ## SCE constructor currently errors on empty rowRanges.
    rowRanges <- rowRanges(se)
    if (!hasLength(rowRanges)) {
        rowRanges <- emptyRanges(names = rownames(se))
    }

    ## Then coerce to SingleCellExperiment.
    ## Note that `as` method isn't currently returning valid.
    sce <- SingleCellExperiment(
        assays = assays(se),
        rowRanges = rowRanges,
        colData = colData(se),
        metadata = metadata(se),
        reducedDims = reducedDims
    )

    ## Optionally, use `isSpike` internally to define the `spikeNames`.
    if (is.character(spikeNames)) {
        for (i in seq_along(spikeNames)) {
            isSpike(sce, spikeNames[[i]]) <- spikeNames[[i]]
        }
    }

    validObject(sce)
    sce
}

args <- c("rowRanges", "colData", "metadata")
formals(makeSingleCellExperiment)[args] <-
    formals(makeSummarizedExperiment)[args]
args <- "reducedDims"
formals(makeSingleCellExperiment)[args] <-
    formals(SingleCellExperiment)[args]
