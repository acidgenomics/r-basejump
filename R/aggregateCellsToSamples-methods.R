#' Aggregate Cells to Samples
#'
#' Utilty function that factilites cell-to-sample aggregation. By default, this
#' function will sum the counts across cells to sample level.
#'
#' This function is intended primarily for quality control analysis.
#'
#' Internally it automatically obtains the cell-to-sample groupings and then
#' performs aggregation with the [aggregateCols()] function.
#'
#' @name aggregateCellsToSamples
#' @family SingleCellExperiment Functions
#' @export
#'
#' @inheritParams general
#'
#' @return `SummarizedExperiment`. Object with cell-level counts aggregated
#'   to sample-level.
#'
#' @examples
#' data(sce_small)
#' x <- aggregateCellsToSamples(sce_small)
#' print(x)
NULL



.aggregateCellsToSamples.SCE <-  # nolint
    function(object, fun) {
        validObject(object)
        fun <- match.arg(fun)
        rse <- as(object, "RangedSummarizedExperiment")
        colData(rse)[["aggregate"]] <- cell2sample(object)
        aggregateCols(
            object = rse,
            col = "aggregate",
            fun = fun
        )
    }
formals(.aggregateCellsToSamples.SCE)[["fun"]] <- .aggregateFuns



#' @rdname aggregateCellsToSamples
#' @export
setMethod(
    f = "aggregateCellsToSamples",
    signature = signature("SingleCellExperiment"),
    definition = .aggregateCellsToSamples.SCE
)
