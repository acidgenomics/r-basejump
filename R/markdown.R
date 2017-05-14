#' Markdown utilities.
#'
#' @rdname markdown
#'
#' @param character Character vector.
#'
#' @examples
#' mdList(c("milk", "eggs"))
#' mdList(c("milk", "eggs"), ordered = TRUE)



#' @rdname markdown
#' @description Markdown list.
#'
#' @param ordered Ordered (`TRUE`) or unordered (`FALSE`) list in Markdown
#'   format.
#'
#' @return Character vector.
#' @export
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



#' @rdname markdown
#' @export
md_list <- mdList
