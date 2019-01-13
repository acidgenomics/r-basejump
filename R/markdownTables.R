#' Multiple Markdown tables
#'
#' Knit multiple tables in a single R Markdown chunk.
#'
#' @note [knitr::kable()] now supports multiple tables as a `list` for the `x`
#'   argument, but it still only supports a single caption. `markdownTables`
#'   extends this functionality, but supporting captions for each table.
#'
#' @export
#'
#' @param list Named `list`.
#'   Column data that can be coerced to `data.frame`.
#' @param captions `character` or `NULL`.
#'   Table captions. If `NULL`, the names of the list will be used automatically
#'   as captions.
#' @param force `logical(1)`.
#'   Force knit output using [knitr::asis_output()].
#'   *Recommended for development and unit testing only.*
#'
#' @return
#' - knit call: `asis_output`
#' - Otherwise: `list`.
#'
#' @seealso
#' - `knitr::kable`.
#' - [Stack Overflow post](https://stackoverflow.com/a/35149103/3911732).
#'
#' @examples
#' list <- list(
#'     mpg = head(ggplot2::mpg),
#'     mtcars = head(datasets::mtcars)
#' )
#' captions <- c(
#'     mpg = "Miles per gallon",
#'     mtcars = "Motor Trend car road tests"
#' )
#' markdownTables(list = list, captions = captions)
markdownTables <- function(
    list,
    captions = NULL,
    force = FALSE
) {
    assert(
        is.list(list),
        isAny(captions, classes = c("character", "NULL"))
    )
    if (is.null(captions)) {
        assert(hasNames(list))
        captions <- names(list)
    }
    assert(
        isCharacter(captions),
        areSameLength(list, captions),
        isFlag(force)
    )
    output <- opts_knit[["get"]]("rmarkdown.pandoc.to")
    if (!is.null(output) || isTRUE(force)) {
        tables <- mapply(
            x = list,
            caption = captions,
            FUN = function(x, caption) {
                kable(x = as.data.frame(x), caption = caption)
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        asis_output(tables)
    } else {
        # Return the unmodified list if not in a knit call.
        list
    }
}




#' @rdname markdownTables
#' @usage NULL
#' @export
mdTables <- markdownTables
