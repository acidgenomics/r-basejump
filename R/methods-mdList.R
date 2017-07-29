#' Markdown List
#'
#' @rdname mdList
#'
#' @param ordered Ordered (`TRUE`; `1.`) or unordered (`FALSE`; `-`) list in
#'   Markdown format.
#'
#' @return [writeLines()].
#' @export
#'
#' @examples
#' mdList(c("milk", "eggs"))
#' mdList(c("milk", "eggs"), ordered = TRUE)
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
    writeLines(lines)
})
