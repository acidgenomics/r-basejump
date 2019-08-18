#' Markdown list
#'
#' Include a Markdown-formatted list, either ordered or unordered. This function
#' works in any R Markdown code block. When calling from inside an `asis` chunk,
#' set `asis = TRUE`.
#'
#' @inherit markdownHeader
#' @note Updated 2019-08-18.
#' @export
#'
#' @inheritParams acidroxygen::params
#'
#' @examples
#' groceries <- c("milk", "eggs")
#' markdownList(groceries)
#' markdownList(groceries, ordered = TRUE)
#' markdownList(groceries, asis = TRUE)
markdownList <- function(
    text,
    ordered = FALSE,
    asis = FALSE
) {
    text <- as.character(text)
    assert(
        isCharacter(text),
        isFlag(ordered),
        isFlag(asis)
    )
    text <- vapply(
        X = seq_along(text),
        FUN = function(a) {
            if (isTRUE(ordered)) {
                prefix <- paste0(a, ".")
            } else {
                prefix <- "-"
            }
            paste(prefix, text[[a]])
        },
        FUN.VALUE = character(1L)
    )
    if (isTRUE(asis)) {
        text <- c("", list, "")
        writeLines(text = text, con = stdout())
    } else {
        ## Add a trailing line break.
        text <- paste0(text, "\n")
        ## Specify that output should be handled as Markdown text.
        text <- structure(text, format = "markdown")
        text <- asis_output(text)
        text
    }
}



#' @rdname markdownList
#' @usage NULL
#' @export
mdList <- markdownList
