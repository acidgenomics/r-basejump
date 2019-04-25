#' @name aggregateCellsToSamples
#' @inherit bioverbs::aggregateCellsToSamples
#'
#' @inheritParams aggregate
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @details
#' Internally [aggregateCellsToSamples()] automatically obtains the
#' cell-to-sample groupings and then performs aggregation with the
#' [aggregateCols()] function.
#'
#' @examples
#' data(sce, package = "acidtest")
#' x <- aggregateCellsToSamples(sce)
#' print(x)
NULL



aggregateCellsToSamples.SingleCellExperiment <-  # nolint
    function(object, fun) {
        validObject(object)
        fun <- match.arg(fun)
        rse <- as(object, "RangedSummarizedExperiment")
        colData <- colData(rse)
        assert(areDisjointSets("aggregate", colnames(colData)))
        colData[["aggregate"]] <- cell2sample(object)
        if ("sampleID" %in% colnames(colData)) {
            colData[["sampleID"]] <- NULL
        }
        colData(rse) <- colData
        aggregateCols(object = rse, col = "aggregate", fun = fun)
    }

formals(aggregateCellsToSamples.SingleCellExperiment)[["fun"]] <- .aggregateFuns



#' @rdname aggregateCellsToSamples
#' @export
setMethod(
    f = "aggregateCellsToSamples",
    signature = signature("SingleCellExperiment"),
    definition = aggregateCellsToSamples.SingleCellExperiment
)
