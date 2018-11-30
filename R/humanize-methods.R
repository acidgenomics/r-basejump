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
#' ## SummarizedExperiment
#' dimnames(rse)
#' x <- humanize(rse)
#'
#' ## SingleCellExperiment
#' dimnames(sce)
#' x <- humanize(sce)
NULL



humanize.SummarizedExperiment <- function(object) {
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
