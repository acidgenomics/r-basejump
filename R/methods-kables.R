#' Create Tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' Handle multiple kables in a single RMarkdown chunk.
#'
#' @rdname kables
#'
#' @param list List of column data (e.g. [data.frame], [matrix]).
#' @param captions Optional character vector of table captions.
#'
#' @return Knit tables, using [knitr::kable()].
#' @export
#'
#' @examples
#' list(starwars, head(mtcars)) %>% kables
setMethod("kables", "list", function(object, captions = NULL) {
    output <- opts_knit[["get"]]("rmarkdown.pandoc.to")
    if (!is.null(output)) {
        tables <- lapply(seq_along(object), function(a) {
            kable(object[a], caption = captions[a])
        })
        asis_output(tables)
    } else {
        # Return the unmodified object if not in a knit call
        object
    }
})
