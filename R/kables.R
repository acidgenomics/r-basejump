#' Create tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' Handle multiple kables in a single RMarkdown chunk
#'
#' @author Michael Steinbaugh
#'
#' @param list List of tables (e.g. data frame, matrix)
#' @param captions Caption character vector
#'
#' @return Knit tables, using \code{knitr::kable()}
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
