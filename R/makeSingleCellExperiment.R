# FIXME Improve the documentation.



#' Make Single-Cell Experiment
#'
#' @family Data Functions
#' @author Michael Steinbaugh
#' @inherit makeSummarizedExperiment
#' @export
#'
#' @inheritParams general
#'
#' @return `SingleCellExperiment`.
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
    sce <- SingleCellExperiment(
        assays = assays(rse),
        rowRanges = rowRanges(rse),
        colData = colData(rse),
        metadata = metadata(rse)
    )

    # Now coerce to SingleCellExperiment.
    # FIXME This isn't returning valid...
    # sce <- as(rse, "SingleCellExperiment")

    # Optionally, use `isSpike` internally to define the `spikeNames`.
    if (is.character(spikeNames)) {
        for (i in seq_along(spikeNames)) {
            isSpike(sce, spikeNames[[i]]) <- spikeNames[[i]]
        }
    }

    validObject(sce)
    sce
}
