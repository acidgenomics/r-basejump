#' Markdown List
#'
#' @rdname markdownList
#' @name markdownList
#' @family Report Utilities
#'
#' @inherit markdownHeader
#'
#' @param ordered Ordered (`TRUE`; `1.`) or unordered (`FALSE`; `-`) list in
#'   Markdown format.
#'
#' @examples
#' groceries <- c("milk", "eggs")
#' markdownList(groceries)
#' markdownList(groceries, ordered = TRUE)
#' markdownList(groceries, asis = TRUE)
NULL



# Constructors =================================================================
#' @importFrom knitr asis_output
.markdownList <- function(
    object,
    ordered = FALSE,
    asis = FALSE) {
    lst <- vapply(seq_along(object), function(a) {
        if (isTRUE(ordered)) {
            prefix <- paste0(a, ".")
        } else {
            prefix <- "-"
        }
        paste(prefix, object[[a]])
    },
    character(1L))
    if (isTRUE(asis)) {
        writeLines(c("", lst, ""))
    } else {
        lst %>%
            # Add a trailing line break
            paste0("\n") %>%
            # Specify that output should be handled as Markdown text
            structure(format = "markdown") %>%
            asis_output()
    }
}



# Methods ======================================================================
#' @rdname markdownList
#' @export
setMethod(
    "markdownList",
    signature("character"),
    .markdownList)
