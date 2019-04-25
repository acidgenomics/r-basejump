#' @name humanize
#' @inherit bioverbs::humanize
#' @inheritParams params
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



humanize.SummarizedExperiment <-  # nolint
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
    definition = humanize.SummarizedExperiment
)
