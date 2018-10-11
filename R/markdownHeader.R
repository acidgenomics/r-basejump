#' Markdown Header
#'
#' Generate a Markdown header (levels 1-7) in any R Markdown code block. When
#' calling inside an `asis` chunk, set `asis = TRUE`.
#'
#' @family Markdown Functions
#' @export
#'
#' @inheritParams general
#' @param text `string`. Header text.
#' @param level `scalar integer`. Header level (1-7).
#' @param tabset `boolean`. Include tabset marker.
#' @param asis `boolean`. Set this to `TRUE` when using the function inside a
#'   loop or inside an R Markdown chunk with '`results = "asis"`' enabled.
#'
#' @seealso
#' [Markdown Syntax](https://daringfireball.net/projects/markdown/syntax).
#'
#' @return
#' - "`asis = TRUE`": [knitr::asis_output()].
#' - "`asis = FALSE`": [writeLines()].
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
    assert_is_a_string(text)
    assert_all_are_non_missing_nor_empty_character(text)
    assertIsHeaderLevel(level)
    assert_is_a_bool(tabset)
    assert_is_a_bool(asis)

    # Add the header level
    header <- paste(str_dup("#", level), text)

    # Append tabset label
    if (isTRUE(tabset)) {
        header <- paste(header, "{.tabset}")
    }

    # Return
    if (isTRUE(asis)) {
        writeLines(c("", "", header, ""))
    } else {
        header %>%
            # Ensure trailing line break
            paste0("\n") %>%
            # Specify that output should be handled as Markdown text
            structure(format = "markdown") %>%
            asis_output()
    }
}



#' @rdname markdownHeader
#' @export
mdHeader <- markdownHeader
