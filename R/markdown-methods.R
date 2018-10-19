#' Markdown
#'
#' @name markdown
#' @family Markdown Functions
#'
#' @inheritParams general
#'
#' @return Markdown output.
#'
#' @examples
#' data(rse_small)
#' markdown(rse_small)
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
