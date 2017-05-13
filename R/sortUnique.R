#' Quickly perform sort unique on a vector.
#'
#' The function also strips \code{NA} values. This is useful for gene list
#' server queries, for example.
#'
#' @param vector Vector with duplicates, \code{NA} values.
#'
#' @return Unique vector.
#' @export
#'
#' @examples
#' sortUnique(c("milk", "eggs", "eggs", NA))
sortUnique <- function(vector) {
    vector %>%
        na.omit %>%
        sort %>%
        unique
}



#' @rdname sortUnique
#' @usage NULL
#' @export
sort_unique <- sortUnique
