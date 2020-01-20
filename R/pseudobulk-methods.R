#' @name pseudobulk
#' @inherit acidgenerics::pseudobulk
#' @note Updated 2019-08-11.
#'
#' @inheritParams aggregate
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' x <- pseudobulk(object)
#' print(x)
NULL



#' @rdname pseudobulk
#' @name pseudobulk
#' @importFrom acidgenerics pseudobulk
#' @usage pseudobulk(object, ...)
#' @export
NULL



## Updated 2019-07-22.
`pseudobulk,SingleCellExperiment` <-  # nolint
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

formals(`pseudobulk,SingleCellExperiment`)[["fun"]] <-
    setdiff(.aggregateFuns, "sum")



#' @rdname pseudobulk
#' @export
setMethod(
    f = "pseudobulk",
    signature = signature("SingleCellExperiment"),
    definition = `pseudobulk,SingleCellExperiment`
)
