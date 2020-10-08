#' @name markdown
#' @inherit AcidGenerics::markdown
#' @note Updated 2020-07-24.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' markdown(rse)
NULL



## Updated 2020-07-24.
`markdown,SummarizedExperiment` <-  # nolint
    function(object) {
        requireNamespaces("knitr")
        x <- object
        x <- sampleData(x)
        x <- as.data.frame(x)
        knitr::kable(x)
    }



#' @rdname markdown
#' @export
setMethod(
    f = "markdown",
    signature = signature("SummarizedExperiment"),
    definition = `markdown,SummarizedExperiment`
)
