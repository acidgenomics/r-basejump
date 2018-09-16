#' Markdown Hyperlink
#'
#' For use in `asis` blocks only.
#'
#' @family R Markdown Functions
#' @author Rory Kirchner, Michael Steinbaugh
#' @export
#'
#' @inherit markdownHeader
#'
#' @param url `string`. URL.
#' @param title `string` or `NULL`. Link title attribute. This will appear in a
#'   mouse-over pop-up box.
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
    assert_is_a_string(text)
    assert_is_a_string(url)
    assertIsAStringOrNULL(title)
    x <- paste0("[", text, "](", url, ")")
    if (!is.null(title)) {
        x <- paste0(x, ": ", title)
    }
    writeLines(x)
}



#' @rdname markdownLink
#' @export
mdLink <- markdownLink
