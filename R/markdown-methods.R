#' Markdown
#'
#' @name markdown
#' @family Markdown Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
NULL



.markdown.SE <-  # nolint
    function(object, headerLevel = 2L) {
        assertIsAHeaderLevel(headerLevel)
        markdownHeader("Sample data", level = headerLevel) %>%
            show()
        sampleData(object) %>%
            as.data.frame() %>%
            kable() %>%
            show()
    }



#' @rdname markdown
#' @export
setMethod(
    f = "markdown",
    signature = signature("SummarizedExperiment"),
    definition = .markdown.SE
)
