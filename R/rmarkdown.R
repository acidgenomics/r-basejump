# Markdown ====
#' Markdown header
#'
#' @param string String.
#' @param level Header level (1-7).
#'
#' @return String.
#' @export
#'
#' @examples
#' mdHeader("Header")
#' mdHeader("Header", level = 4L)
mdHeader <- function(string, level = 2L) {
    if (!level %in% seq(1:7)) {
        stop("Markdown supports 1-7 header levels")
    }
    paste(str_dup("#", level), string)
}



#' Markdown list
#'
#' @param vec Character vector.
#' @param ordered Ordered (`TRUE`; `1.`) or unordered (`FALSE`; `-`) list in
#'   Markdown format.
#' @export
#'
#' @examples
#' mdList(c("milk", "eggs"))
#' mdList(c("milk", "eggs"), ordered = TRUE)
mdList <- function(vec, ordered = FALSE) {
    if (!is.character(vec)) {
        stop("A character vector is required.")
    }
    lines <- vapply(seq_along(vec), function(a) {
        if (isTRUE(ordered)) {
            prefix <- str_c(a, ".")
        } else {
            prefix <- "-"
        }
        paste(prefix, vec[[a]])
    },
    character(1L))
    writeLines(lines)
}



# knitr ====
#' Create tables in LaTeX, HTML, Markdown and reStructuredText
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
#' list(starwars, head(mtcars)) %>% kables
kables <- function(list, captions = NULL) {
    output <- opts_knit[["get"]]("rmarkdown.pandoc.to")
    if (!is.null(output)) {
        tables <- lapply(seq_along(list), function(a) {
            kable(list[a], caption = captions[a])
        })
        asis_output(tables)
    } else {
        list
    }
}
