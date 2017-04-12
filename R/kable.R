#' Create tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' Wrapper for \code{knitr::kable}. Improves appearance in RStudio RMarkdown
#' chunks by returning a data frame instead of a kable outside of a knit.
#' Customizes knit output defaults based on format.
#'
#' @author Michael Steinbaugh
#'
#' @param x An R object (typically a matrix or data frame)
#' @param ... Other arguments supported by \code{knitr::kable}
#'
#' @return Kable during knit, otherwise the original data
#' @export
#'
#' @examples
#' kable(head(iris))
kable <- function(x, ...) {
    output <- opts_knit$get("rmarkdown.pandoc.to")
    if (is.null(output)) {
        return(x)
    } else {
        kable(x, row.names = FALSE, ...)
    }
}
# For PDF: output == "latex"
