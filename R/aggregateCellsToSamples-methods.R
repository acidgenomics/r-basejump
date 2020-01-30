#' @name aggregateCellsToSamples
#' @inherit acidgenerics::aggregateCellsToSamples
#' @note Updated 2020-01-30.
#'
#' @inheritParams aggregate
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @details
#' Internally [aggregateCellsToSamples()] automatically obtains the
#' cell-to-sample groupings and then performs aggregation with the
#' [aggregateCols()] function.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#'
#' ## SingleCellExperiment ====
#' x <- SingleCellExperiment
#' x <- aggregateCellsToSamples(x)
#' print(x)
NULL



#' @rdname aggregateCellsToSamples
#' @name aggregateCellsToSamples
#' @importFrom acidgenerics aggregateCellsToSamples
#' @usage aggregateCellsToSamples(x, ...)
#' @export
NULL



## Updated 2020-01-30.
`aggregateCellsToSamples,SingleCellExperiment` <-  # nolint
    function(x, FUN) {
        validObject(x)
        FUN <- match.fun(FUN)
        rse <- as(x, "RangedSummarizedExperiment")
        colData <- colData(rse)
        assert(areDisjointSets("aggregate", colnames(colData)))
        colData[["aggregate"]] <- cell2sample(x)
        if ("sampleID" %in% colnames(colData)) {
            colData[["sampleID"]] <- NULL
        }
        colData(rse) <- colData
        aggregateCols(x = rse, col = "aggregate", FUN = FUN)
    }



#' @rdname aggregateCellsToSamples
#' @export
setMethod(
    f = "aggregateCellsToSamples",
    signature = signature("SingleCellExperiment"),
    definition = `aggregateCellsToSamples,SingleCellExperiment`
)
