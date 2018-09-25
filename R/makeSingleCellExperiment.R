# FIXME Improve the documentation.



#' Make Single-Cell Experiment
#'
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
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

    # Now coerce to SingleCellExperiment.
    sce <- as(rse, "SingleCellExperiment")

    # Optionally, use `isSpike` internally to define the `spikeNames`.
    if (is.character(spikeNames)) {
        for (i in seq_along(spikeNames)) {
            isSpike(sce, spikeNames[[i]]) <- spikeNames[[i]]
        }
    }

    sce
}
