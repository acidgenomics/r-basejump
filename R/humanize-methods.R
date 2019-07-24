#' @name humanize
#' @inherit bioverbs::humanize
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#' rse <- RangedSummarizedExperiment
#' sce <- SingleCellExperiment
#'
#' ## SummarizedExperiment ====
#' lapply(dimnames(rse), head)
#' x <- humanize(rse)
#' lapply(dimnames(x), head)
#'
#'
#' ## SingleCellExperiment ====
#' lapply(dimnames(sce), head)
#' x <- humanize(sce)
#' lapply(dimnames(x), head)
NULL



#' @rdname humanize
#' @name humanize
#' @importFrom bioverbs humanize
#' @usage humanize(object, ...)
#' @export
NULL



## Updated 2019-07-22.
`humanize,SummarizedExperiment` <-  # nolint
    function(object) {
        message("Making the rownames and colnames human readable.")
        to <- object
        to <- convertSampleIDsToNames(to)
        to <- convertGenesToSymbols(to)
        to
    }



#' @rdname humanize
#' @export
setMethod(
    f = "humanize",
    signature = signature("SummarizedExperiment"),
    definition = `humanize,SummarizedExperiment`
)
