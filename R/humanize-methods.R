#' @name humanize
#' @inherit bioverbs::humanize
#' @inheritParams params
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



#' @importFrom bioverbs humanize
#' @aliases NULL
#' @export
bioverbs::humanize



humanize.SummarizedExperiment <-  # nolint
    function(object) {
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
