#' Markdown header
#'
#' Generate a Markdown header (levels 1-7) in any R Markdown code block. When
#' calling inside an `asis` chunk, set `asis = TRUE`.
#'
#' @note Updated 2020-07-24.
#' @export
#'
#' @inheritParams acidroxygen::params
#' @param level `integer(1)`.
#'   Markdown header level (1-7).
#'
#' @seealso
#' [Markdown Syntax](https://daringfireball.net/projects/markdown/syntax).
#'
#' @return
#' - `asis = TRUE`: [knitr::asis_output()] return.
#' - `asis = FALSE`: [`writeLines()`][base::writeLines] return.
#'
#' @examples
#' markdownHeader("Header", level = 2L)
#' markdownHeader("Header", tabset = TRUE)
#' markdownHeader("Header", asis = TRUE)
markdownHeader <- function(
    text,
    level = 2L,
    tabset = FALSE,
    asis = FALSE
) {
    assert(
        isString(text),
        isHeaderLevel(level),
        isFlag(tabset),
        isFlag(asis)
    )
    ## Add the header level.
    text <- paste(str_dup("#", level), text)
    ## Append tabset label
    if (isTRUE(tabset)) {
        text <- paste(text, "{.tabset}")
    }
    ## Return.
    if (isTRUE(asis)) {
        text <- c("", "", text, "")
        writeLines(text = text, con = stdout())
    } else {
        ## Ensure trailing line break.
        text <- paste0(text, "\n")
        ## Specify that output should be handled as Markdown text.
        text <- structure(text, format = "markdown")
        requireNamespaces("knitr")
        text <- knitr::asis_output(text)
        text
    }
}



#' @rdname markdownHeader
#' @usage NULL
#' @export
mdHeader <- markdownHeader
