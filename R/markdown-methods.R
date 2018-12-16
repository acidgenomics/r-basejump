#' Markdown
#'
#' @name markdown
#' @inheritParams params
#'
#' @return Markdown output.
#'
#' @examples
#' data(rse)
#' markdown(rse)
NULL



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
