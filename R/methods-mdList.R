#' Markdown List
#'
#' @rdname mdList
#' @name mdList
#' @family Report Utilities
#'
#' @inherit mdHeader
#'
#' @param ordered Ordered (`TRUE`; `1.`) or unordered (`FALSE`; `-`) list in
#'   Markdown format.
#'
#' @examples
#' groceries <- c("milk", "eggs")
#' mdList(groceries)
#' mdList(groceries, ordered = TRUE)
#' mdList(groceries, asis = TRUE)
NULL



# Constructors ====
#' @importFrom knitr asis_output
.mdList <- function(
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



# Methods ====
#' @rdname mdList
#' @export
setMethod(
    "mdList",
    signature("character"),
    .mdList)
