# Consider renaming "link" to "url"?
# Improve the return documentation consistency across Markdown functions



#' Markdown Link
#'
#' For use in 'asis' blocks.
#'
#' @family R Markdown Functions
#' @author Rory Kirchner
#'
#' @param text Link text.
#' @param url Link URL.
#' @param title *Optional.* Link title attribute. This will appear in mouse-over
#'   popup.
#'
#' @seealso [Markdown Syntax Documentation](https://daringfireball.net/projects/markdown/syntax).
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
markdownLink <- function(text, url, title = NULL) {
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
