#' Markdown hyperlink
#'
#' For use in `asis` blocks only.
#'
#' @inherit markdownHeader
#' @author Rory Kirchner, Michael Steinbaugh
#' @note Updated 2019-08-19.
#' @export
#'
#' @inheritParams acidroxygen::params
#'
#' @return Markdown-formatted link.
#'
#' @examples
#' markdownLink(
#'     text = "R",
#'     url = "https://www.r-project.org",
#'     title = "The R Project for Statistical Computing"
#' )
markdownLink <- function(
    text,
    url,
    title = NULL
) {
    assert(
        isString(text),
        isString(url),
        isString(title, nullOK = TRUE)
    )
    text <- paste0("[", text, "](", url, ")")
    if (!is.null(title)) {
        text <- paste0(text, ": ", title)
    }
    writeLines(text = text, con = stdout())
}



#' @rdname markdownLink
#' @usage NULL
#' @export
mdLink <- markdownLink
