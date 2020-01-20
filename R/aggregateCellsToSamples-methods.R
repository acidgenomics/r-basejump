#' @name aggregateCellsToSamples
#' @inherit acidgenerics::aggregateCellsToSamples
#' @note Updated 2019-07-28.
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
#' object <- SingleCellExperiment
#' x <- aggregateCellsToSamples(object)
#' print(x)
NULL



#' @rdname aggregateCellsToSamples
#' @name aggregateCellsToSamples
#' @importFrom acidgenerics aggregateCellsToSamples
#' @usage aggregateCellsToSamples(object, ...)
#' @export
NULL



## Updated 2019-07-22.
`aggregateCellsToSamples,SingleCellExperiment` <-  # nolint
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

formals(`aggregateCellsToSamples,SingleCellExperiment`)[["fun"]] <-
    .aggregateFuns



#' @rdname aggregateCellsToSamples
#' @export
setMethod(
    f = "aggregateCellsToSamples",
    signature = signature("SingleCellExperiment"),
    definition = `aggregateCellsToSamples,SingleCellExperiment`
)
