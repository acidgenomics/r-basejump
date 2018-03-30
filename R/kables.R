#' Create Tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' Handle multiple kables in a single R Markdown chunk.
#'
#' @family R Markdown Functions
#'
#' @param list List of column data (e.g. `data.frame`, `matrix`).
#' @param captions Optional character vector of table captions.
#' @param force Force kable output. Recommended for development and unit testing
#'   only.
#'
#' @return Knit tables, using [knitr::kable()].
#' @export
#'
#' @seealso [Stack Overflow post](https://stackoverflow.com/a/35149103/3911732).
#'
#' @examples
#' list(head(starwars), head(mtcars)) %>% kables()
kables <- function(
    list,
    captions = NULL,
    force = FALSE
) {
    assert_is_list(list)
    assertIsCharacterOrNULL(captions)
    if (is.character(captions)) {
        assert_all_are_same_length(list, captions)
    }
    assert_is_a_bool(force)
    output <- opts_knit[["get"]]("rmarkdown.pandoc.to")
    if (!is.null(output) || isTRUE(force)) {
        tables <- mapply(
            x = list,
            caption = captions,
            FUN = function(x, caption) {
                kable(x, caption = caption)
            },
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        )
        asis_output(tables)
    } else {
        # Return the unmodified list if not in a knit call
        list
    }
}

