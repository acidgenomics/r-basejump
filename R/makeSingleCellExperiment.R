#' Make Single-Cell Experiment
#'
#' @family Single-Cell Functions
#' @author Michael Steinbaugh
#' @inherit makeSummarizedExperiment
#' @export
#'
#' @inheritParams general
#'
#' @return `SingleCellExperiment`.
#'
#' @examples
#' object <- sce_small
#' assays <- assays(object)
#' rowRanges <- rowRanges(object)
#' colData <- colData(object)
#' metadata <- metadata(object)
#'
#' x <- makeSingleCellExperiment(
#'     assays = assays,
#'     rowRanges = rowRanges,
#'     colData = colData,
#'     metadata = metadata
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
    # Make RangedSummarizedExperiment first.
    # Supports automatic resizing of rowRanges and helps slot FASTA spike-ins.
    rse <- makeSummarizedExperiment(
        assays = assays,
        rowRanges = rowRanges,
        colData = colData,
        metadata = metadata,
        transgeneNames = transgeneNames,
        spikeNames = spikeNames
    )

    # Then coerce to SingleCellExperiment.
    # Note that `as()` method isn't currently returning valid.
    sce <- SingleCellExperiment(
        assays = assays(rse),
        rowRanges = rowRanges(rse),
        colData = colData(rse),
        metadata = metadata(rse)
    )

    # Optionally, use `isSpike` internally to define the `spikeNames`.
    if (is.character(spikeNames)) {
        for (i in seq_along(spikeNames)) {
            isSpike(sce, spikeNames[[i]]) <- spikeNames[[i]]
        }
    }

    validObject(sce)
    sce
}
