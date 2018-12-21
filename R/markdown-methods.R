#' @name markdown
#' @inherit bioverbs::markdown
#' @inheritParams params
#' @examples
#' data(rse)
#' markdown(rse)
NULL



#' @importFrom bioverbs markdown
#' @aliases NULL
#' @export
bioverbs::markdown



markdown.SummarizedExperiment <-  # nolint
    function(object) {
        sampleData(object) %>%
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
