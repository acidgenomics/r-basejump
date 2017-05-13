#' Create tables in LaTeX, HTML, Markdown and reStructuredText.
#'
#' Handle multiple kables in a single RMarkdown chunk.
#'
#' @param list List of tables (e.g. data frame, matrix).
#' @param captions Caption character vector.
#'
#' @return Knit tables, using [knitr::kable()].
#' @export
#'
#' @examples
#' list(
#'     starwars,
#'     head(mtcars)
#' ) %>% kables
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
