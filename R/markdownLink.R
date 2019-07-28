#' Markdown hyperlink
#'
#' For use in `asis` blocks only.
#'
#' @inherit markdownHeader
#' @author Rory Kirchner, Michael Steinbaugh
#' @note Updated 2019-07-28.
#' @export
#'
#' @param url `character(1)`.
#'   URL.
#' @param title `character(1)` or `NULL`.
#'   Link title attribute. This will appear in a mouse-over pop-up box.
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
    x <- paste0("[", text, "](", url, ")")
    if (!is.null(title)) {
        x <- paste0(x, ": ", title)
    }
    writeLines(x)
}



#' @rdname markdownLink
#' @usage NULL
#' @export
mdLink <- markdownLink
