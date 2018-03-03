#' Create Tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' Handle multiple kables in a single RMarkdown chunk.
#'
#' @rdname kables
#' @name kables
#' @family R Markdown Functions
#'
#' @inheritParams general
#'
#' @param list List of column data (e.g. [data.frame], [matrix]).
#' @param captions Optional character vector of table captions.
#' @param force Force kable output. *Recommended for development and unit
#'   testing only.*
#'
#' @return Knit tables, using [knitr::kable()].
#' @export
#'
#' @seealso [Stack Overflow](https://stackoverflow.com/a/35149103/3911732).
#'
#' @examples
#' list(head(starwars), head(mtcars)) %>% kables()
NULL



# Methods ======================================================================
#' @rdname kables
#' @importFrom knitr asis_output kable opts_knit
#' @export
setMethod(
    "kables",
    signature("list"),
    function(
        object,
        captions = NULL,
        force = FALSE
    ) {
        assert_is_any_of(captions, c("character", "NULL"))
        if (is.character(captions)) {
            assert_are_identical(length(object), length(captions))
        }
        assert_is_a_bool(force)

        output <- opts_knit[["get"]]("rmarkdown.pandoc.to")

        if (!is.null(output) || isTRUE(force)) {
            tables <- lapply(seq_along(object), function(a) {
                kable(object[a], caption = captions[a])
            })
            asis_output(tables)
        } else {
            # Return the unmodified object if not in a knit call
            object
        }
    }
)
