#' Markdown Header
#'
#' @name markdownHeader
#' @family R Markdown Functions
#' @author Michael Steinbaugh
#'
#' @param object `scalar`, which will be coerced to `character`.
#' @param level Header level (1-7).
#' @param tabset Include tabset marker.
#' @param asis Set this to `TRUE` when using the function inside a loop or
#'   inside an RMarkdown chunk with '`results="asis"`' enabled.
#'
#' @return
#' - "`asis = TRUE`": [knitr::asis_output()].
#' - "`asis = FALSE`": [writeLines()].
#' @export
#'
#' @examples
#' markdownHeader("Header")
#' markdownHeader("Header", level = 4L)
#' markdownHeader("Header", tabset = TRUE)
#' markdownHeader("Header", asis = TRUE)
markdownHeader <- function(
    object,
    level = 2L,
    tabset = FALSE,
    asis = FALSE
) {
    assert_is_scalar(object)
    object <- as.character(object)
    assert_all_are_not_na(object)
    assert_all_are_non_missing_nor_empty_character(object)
    assertIsAHeaderLevel(level)
    assert_is_a_bool(tabset)
    assert_is_a_bool(asis)

    # Add the header level
    header <- paste(str_dup("#", level), object)

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



# Aliases ======================================================================
#' @rdname markdownHeader
#' @usage NULL
#' @export
markdownHeader -> mdHeader
