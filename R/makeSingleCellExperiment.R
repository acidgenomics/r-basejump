#' Make a SingleCellExperiment object
#'
#' @export
#' @inherit makeSummarizedExperiment
#' @inheritParams params
#'
#' @return `SingleCellExperiment`.
#'
#' @examples
#' data(sce, package = "acidtest")
#' object <- sce
#' x <- makeSingleCellExperiment(
#'     assays = SummarizedExperiment::assays(object),
#'     rowRanges = SummarizedExperiment::rowRanges(object),
#'     colData = SummarizedExperiment::colData(object),
#'     metadata = S4Vectors::metadata(object)
#' )
#' print(x)
makeSingleCellExperiment <- function(
    assays,
    rowRanges,
    colData,
    metadata,
    transgeneNames = NULL,
    spikeNames = NULL
) {
    ## Make RangedSummarizedExperiment first.
    ## Supports automatic resizing of rowRanges and helps slot FASTA spike-ins.
    rse <- makeSummarizedExperiment(
        assays = assays,
        rowRanges = rowRanges,
        colData = colData,
        metadata = metadata,
        transgeneNames = transgeneNames,
        spikeNames = spikeNames
    )

    ## Then coerce to SingleCellExperiment.
    ## Note that `as` method isn't currently returning valid.
    sce <- SingleCellExperiment(
        assays = assays(rse),
        rowRanges = rowRanges(rse),
        colData = colData(rse),
        metadata = metadata(rse)
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
