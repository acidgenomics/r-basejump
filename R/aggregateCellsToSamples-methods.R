#' @name aggregateCellsToSamples
#' @inherit AcidGenerics::aggregateCellsToSamples
#' @note Updated 2020-01-30.
#'
#' @inheritParams aggregate
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @details
#' Internally [aggregateCellsToSamples()] automatically obtains the
#' cell-to-sample groupings and then performs a sum aggregation with the
#' [aggregateCols()] function.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' x <- SingleCellExperiment
#' x <- aggregateCellsToSamples(x)
#' print(x)
NULL



## Updated 2021-01-16.
`aggregateCellsToSamples,SingleCellExperiment` <-  # nolint
    function(x) {
        validObject(x)
        rse <- as(x, "RangedSummarizedExperiment")
        colData <- colData(rse)
        assert(areDisjointSets("aggregate", colnames(colData)))
        colData[["aggregate"]] <- cell2sample(x)
        sampleCol <- matchSampleColumn(colData)
        if (isSubset(sampleCol, colnames(colData))) {
            colData[[sampleCol]] <- NULL
        }
        colData(rse) <- colData
        aggregateCols(x = rse, col = "aggregate", fun = "sum")
    }



#' @rdname aggregateCellsToSamples
#' @export
setMethod(
    f = "aggregateCellsToSamples",
    signature = signature("SingleCellExperiment"),
    definition = `aggregateCellsToSamples,SingleCellExperiment`
)
