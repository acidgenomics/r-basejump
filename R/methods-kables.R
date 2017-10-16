#' Create Tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' Handle multiple kables in a single RMarkdown chunk.
#'
#' @rdname kables
#' @name kables
#' @family Report Utilities
#'
#' @inheritParams AllGenerics
#'
#' @param list List of column data (e.g. [data.frame], [matrix]).
#' @param captions Optional character vector of table captions.
#' @param force Force kable output.
#'
#' @return Knit tables, using [knitr::kable()].
#' @export
#'
#' @seealso https://stackoverflow.com/a/35149103/3911732.
#'
#' @examples
#' list(head(starwars), head(mtcars)) %>%
#'     kables()
NULL



# Methods ====
#' @rdname kables
#' @importFrom knitr asis_output kable opts_knit
#' @export
setMethod(
    "kables",
    signature("list"),
    function(
        object,
        captions = NULL,
        force = FALSE) {
        output <- opts_knit[["get"]]("rmarkdown.pandoc.to")
        if (!is.null(output) | isTRUE(force)) {
            tables <- lapply(seq_along(object), function(a) {
                kable(object[a], caption = captions[a])
            })
            asis_output(tables)
        } else {
            # Return the unmodified object if not in a knit call
            object
        }
    })
