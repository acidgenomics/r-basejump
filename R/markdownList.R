#' Markdown list
#' @export
#' @param character \code{character} vector
#' @param ordered Ordered \code{TRUE} or unordered \code{FALSE} list in Markdown format
#' @return \code{character} vector
#' @examples
#' markdownList(c("item1", "item2"), ordered = TRUE)
markdownList <- function(character, ordered = FALSE) {
    if (!is.character(character)) {
        stop("A character vector is required.")
    }
    string <- sapply(seq_along(character), function(a) {
        if (isTRUE(ordered)) {
            prefix <- paste0(a, ".")
        } else {
            prefix <- "-"
        }
        paste0(prefix, " ", character[a])
    })
    return(cat(paste(string, collapse = "\n")))
}
