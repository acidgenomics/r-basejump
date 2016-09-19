#' Markdown list
#'
#' @import magrittr
#'
#' @param character \code{character} vector.
#' @param ordered Ordered \code{TRUE} or unordered \code{FALSE} list in Markdown format.
#'
#' @return Character vector.
#' @export
#'
#' @examples
#' mdlist(c("item1", "item2"), ordered = TRUE)
mdlist <- function(character, ordered = FALSE) {
    if (isTRUE(ordered)) {
        prefix <- "0."
    } else {
        prefix <- "-"
    }
    character %>%
        paste0(prefix, " ", .) %>%
        paste(., collapse = "\n")
}
