#' @name humanize
#' @inherit AcidGenerics::humanize
#' @note Updated 2020-01-20.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' lapply(dimnames(object), head)
#' x <- humanize(object)
#' lapply(dimnames(x), head)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' lapply(dimnames(object), head)
#' x <- humanize(object)
#' lapply(dimnames(x), head)
NULL



## Updated 2020-01-20.
`humanize,SummarizedExperiment` <-  # nolint
    function(object) {
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
