#' @name humanize
#' @inherit bioverbs::humanize
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(rse, sce, package = "acidtest")
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



## FIXME Check SCE method here.
## Updated 2019-07-22.
`humanize,SummarizedExperiment` <-  # nolint
    function(object) {
        message("Making the rownames and colnames human readable.")
        human <- object
        human <- convertGenesToSymbols(human)
        human <- convertSampleIDsToNames(human)
        human
    }



#' @rdname humanize
#' @export
setMethod(
    f = "humanize",
    signature = signature("SummarizedExperiment"),
    definition = `humanize,SummarizedExperiment`
)
