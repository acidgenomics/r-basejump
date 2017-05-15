## Markdown ====
#' Markdown utilities.
#'
#' @rdname markdown
#'
#' @param character Character vector.
#'
#' @examples
#' mdList(c("milk", "eggs"))
#' mdList(c("milk", "eggs"), ordered = TRUE)



#' @rdname markdown
#' @aliases md_list
#' @description Markdown list.
#'
#' @param ordered Ordered (`TRUE`; `1.`) or unordered (`FALSE`; `-`) list in
#'   Markdown format.
#'
#' @return Character vector.
#' @export
mdList <- function(character, ordered = FALSE) {
    if (!is.character(character)) {
        stop("A character vector is required.")
    }
    string <- sapply(seq_along(character), function(a) {
        if (isTRUE(ordered)) {
            prefix <- paste0(a, ".")
        } else {
            prefix <- "-"
        }
        paste(prefix, character[a])
    })
    return(writeLines(string))
}






## knitr ====
#' Create tables in LaTeX, HTML, Markdown and reStructuredText.
#'
#' Handle multiple kables in a single RMarkdown chunk.
#'
#' @param list List of column data (e.g. data frame, matrix).
#' @param captions Optional character vector of table captions.
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
