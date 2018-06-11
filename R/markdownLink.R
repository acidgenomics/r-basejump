#' Markdown Link
#'
#' For use in 'asis' blocks only.
#'
#' @family R Markdown Functions
#' @author Rory Kirchner, Michael Steinbaugh
#'
#' @inherit markdownHeader
#'
#' @param url URL.
#' @param title *Optional.* Link title attribute. This will appear in a
#'   mouse-over pop-up box.
#'
#' @return Markdown-formatted link.
#' @export
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
    x <- paste0('[', text, '](', url, ')')
    if (!is.null(title)) {
        x <- paste0(x, ': ', title)
    }
    writeLines(x)
}



# Aliases ======================================================================
#' @rdname markdownHeader
#' @usage NULL
#' @export
markdownLink -> mdLink
