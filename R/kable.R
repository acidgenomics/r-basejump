#' Create tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' Wrapper function for \code{knitr::kable()}
#'
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @importFrom knitr kable opts_knit
#'
#' @param x An R object (typically a matrix or data frame)
#' @param ... Other arguments
#'
#' @return Kable during knit (via \code{rmarkdown::render()}), otherwise the
#'   original data
#' @export
#'
#' @examples
#' kable(head(iris))
kable <- function(x, ...) {
    # Improve appearance in RStudio RMarkdown chunks
    output <- knitr::opts_knit$get("rmarkdown.pandoc.to")
    if (!is.null(output)) {
        # Customize output defaults based on format
        if (output == "pdf") {
            return(knitr::kable(x, ...,
                                booktabs = TRUE,
                                longtable = TRUE))
        } else {
            return(knitr::kable(x, ...))
        }
    } else {
        return(x)
    }
}
