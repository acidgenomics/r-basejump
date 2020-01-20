#' @name markdown
#' @inherit acidgenerics::markdown
#' @note Updated 2019-07-28.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' markdown(rse)
NULL



#' @rdname markdown
#' @name markdown
#' @importFrom acidgenerics markdown
#' @usage markdown(object, ...)
#' @export
NULL



## Updated 2019-07-22.
`markdown,SummarizedExperiment` <-  # nolint
    function(object) {
        object %>%
            sampleData() %>%
            as.data.frame() %>%
            kable()
    }



#' @rdname markdown
#' @export
setMethod(
    f = "markdown",
    signature = signature("SummarizedExperiment"),
    definition = `markdown,SummarizedExperiment`
)
