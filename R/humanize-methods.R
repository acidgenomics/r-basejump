#' @name humanize
#' @inherit bioverbs::humanize
#' @note Updated 2019-07-28.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
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
