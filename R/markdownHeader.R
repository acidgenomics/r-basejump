#' Markdown header
#'
#' Generate a Markdown header (levels 1-7) in any R Markdown code block. When
#' calling inside an `asis` chunk, set `asis = TRUE`.
#'
#' @note Updated 2019-08-11.
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
#' - "`asis = TRUE`": [knitr::asis_output()] return.
#' - "`asis = FALSE`": [`writeLines()`][base::writeLines] return.
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
    header <- paste(str_dup("#", level), text)
    ## Append tabset label
    if (isTRUE(tabset)) {
        header <- paste(header, "{.tabset}")
    }
    ## Return.
    if (isTRUE(asis)) {
        writeLines(c("", "", header, ""))
    } else {
        header %>%
            ## Ensure trailing line break.
            paste0("\n") %>%
            ## Specify that output should be handled as Markdown text.
            structure(format = "markdown") %>%
            asis_output()
    }
}



#' @rdname markdownHeader
#' @usage NULL
#' @export
mdHeader <- markdownHeader
