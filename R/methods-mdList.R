#' Markdown List
#'
#' @rdname mdList
#' @name mdList
#'
#' @param ordered Ordered (`TRUE`; `1.`) or unordered (`FALSE`; `-`) list in
#'   Markdown format.
#'
#' @return [writeLines()].
#' @export
#'
#' @examples
#' groceries <- c("milk", "eggs")
#' mdList(groceries)
#' mdList(groceries, ordered = TRUE)
NULL



# Methods ====
#' @rdname mdList
#' @export
setMethod("mdList", "character", function(object, ordered = FALSE) {
    vapply(seq_along(object), function(a) {
        if (isTRUE(ordered)) {
            prefix <- paste0(a, ".")
        } else {
            prefix <- "-"
        }
        paste(prefix, object[[a]])
    },
    character(1L)) %>%
        # Add a trailing line break
        paste0("\n") %>%
        # Specify that output should be handled as Markdown text
        structure(format = "markdown") %>%
        asis_output
})
