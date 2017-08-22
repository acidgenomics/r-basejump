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
    if (!is.character(object)) {
        stop("A character vector is required.")
    }
    lines <- vapply(seq_along(object), function(a) {
        if (isTRUE(ordered)) {
            prefix <- paste0(a, ".")
        } else {
            prefix <- "-"
        }
        paste(prefix, object[[a]])
    },
    character(1L))
    writeLines(c(
        "",
        lines,
        ""))
})
