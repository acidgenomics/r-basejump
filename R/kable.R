#' Create tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' @rdname kable
#'
#' @author Michael Steinbaugh



#' @rdname kable
#' @description Wrapper for \code{knitr::kable} (knit table). Improves
#'   appearance in RStudio RMarkdown chunks by returning a data frame instead of
#'   a kable outside of a knit. Customizes knit output defaults based on format.
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



#' @rdname kable
#' @description Handle multiple kables in a single RMarkdown chunk
#'
#' @param list List of tables (e.g. data frame, matrix)
#' @param captions Caption character vector
#'
#' @return Knit tables, using \code{kable}
#' @export
#'
#' @examples
#' kables(list(iris, mtcars))
kables <- function(list, captions = NULL) {
    output <- opts_knit$get("rmarkdown.pandoc.to")
    if (!is.null(output)) {
        tables <- lapply(seq_along(list), function(a) {
            kable(list[a], caption = captions[a])
        })
        return(asis_output(tables))
    } else {
        return(list)
    }
}
