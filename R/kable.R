#' Create tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' Wrapper for \code{knitr::kable}. Improves appearance in RStudio RMarkdown
#' chunks by returning a data frame instead of a kable outside of a knit.
#' Customizes knit output defaults based on format matches. The function applies
#' grep matching to both default and custom document templates by format (e.g.
#' \code{BiocStyle::pdf_document2}).
#'
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @importFrom knitr kable opts_knit
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
    if (!is.null(output)) {
        if (grepl("pdf_document", output)) {
            return(knitr::kable(x,
                                digits = 2,
                                row.names = FALSE,
                                booktabs = TRUE,
                                longtable = TRUE,
                                ...))
        } else {
            return(knitr::kable(x, ...))
        }
    } else {
        return(x)
    }
}
