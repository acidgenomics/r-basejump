#' @name markdown
#' @inherit bioverbs::markdown
#' @inheritParams params
#' @examples
#' data(rse, package = "acidtest")
#' markdown(rse)
NULL



markdown.SummarizedExperiment <-  # nolint
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
    definition = markdown.SummarizedExperiment
)
