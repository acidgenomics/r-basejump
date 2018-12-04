#' Humanize an R Object
#'
#' @note This can make dimnames invalid (see `make.names()`) and should only
#' be called prior to writing files to disk.
#'
#' @name humanize
#' @inheritParams params
#'
#' @return Modified object, with human-friendly rownames (e.g. gene symbols
#'   instead of stable gene IDs) and colnames (e.g. sample names instead of
#'   sample IDs).
#'
#' @examples
#' data(rse, sce)
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



humanize.SummarizedExperiment <- function(object) {
    message("Making the rownames and colnames human readable.")
    object %>%
        convertGenesToSymbols() %>%
        convertSampleIDsToNames()
}



#' @rdname humanize
#' @export
setMethod(
    f = "humanize",
    signature = signature("SummarizedExperiment"),
    definition = humanize.SummarizedExperiment
)
