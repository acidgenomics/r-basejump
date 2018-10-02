#' Aggregate Cells to Samples
#'
#' @name aggregateCellsToSamples
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `RangedSummarizedExperiment`.
#'
#' @examples
#' x <- aggregateCellsToSamples(sce_small)
#' print(x)
NULL



.aggregateCellsToSamples.SCE <-  # nolint
    function(object) {
        validObject(object)
        rse <- as(object, "RangedSummarizedExperiment")
        colData(rse)[["aggregate"]] <- cell2sample(object)
        aggregateCols(rse)
    }



#' @rdname aggregateCellsToSamples
#' @export
setMethod(
    f = "aggregateCellsToSamples",
    signature = signature("SingleCellExperiment"),
    definition = .aggregateCellsToSamples.SCE
)
