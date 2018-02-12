#' Markdown Header
#'
#' @rdname markdownHeader
#' @name markdownHeader
#' @family Report Utilities
#'
#' @inheritParams general
#'
#' @param level Header level (1-7).
#' @param tabset Include tabset marker.
#' @param asis Set this to `TRUE` when using the function inside a loop or
#'   inside an RMarkdown chunk with `results="asis"` enabled.
#'
#' @return Markdown formatted code as [knitr::asis_output] or
#'   [base::writeLines()].
#'
#' @examples
#' markdownHeader("Header")
#' markdownHeader("Header", level = 4L)
#' markdownHeader("Header", tabset = TRUE)
#' markdownHeader("Header", asis = TRUE)
NULL



# Constructors =================================================================
#' @importFrom knitr asis_output
#' @importFrom stringr str_dup
.markdownHeader <- function(
    object,
    level = 2L,
    tabset = FALSE,
    asis = FALSE) {
    assert_is_a_string(object)
    assert_all_are_not_na(object)
    assert_all_are_non_missing_nor_empty_character(object)
    assert_formal_header_level(level)
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



# Methods ======================================================================
#' @rdname markdownHeader
#' @export
setMethod(
    "markdownHeader",
    signature("character"),
    .markdownHeader)
