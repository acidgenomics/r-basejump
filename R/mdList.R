#' Markdown list
#'
#' @author Michael Steinbaugh
#'
#' @param character Character vector
#' @param ordered Ordered \code{TRUE} or unordered \code{FALSE} list in Markdown
#'   format
#'
#' @return Character vector
#' @export
#'
#' @examples
#' mdList(c("milk", "eggs"))
#' mdList(c("milk", "eggs"), ordered = TRUE)
mdList <- function(character, ordered = FALSE) {
    if (!is.character(character)) {
        stop("A character vector is required.")
    }
    string <- sapply(seq_along(character), function(a) {
        if (isTRUE(ordered)) {
            prefix <- paste0(a, ".")
        } else {
            prefix <- "-"
        }
        paste(prefix, character[a])
    })
    return(writeLines(string))
}
