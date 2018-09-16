#' Markdown
#'
#' @name markdown
#' @family Markdown Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return Markdown output.
#'
#' @examples
#' markdown(rse_small)
NULL



.markdown.SE <-  # nolint
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
    definition = .markdown.SE
)
