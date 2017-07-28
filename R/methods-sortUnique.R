#' Quickly Perform Sort Unique on a Vector
#'
#' The function also strips `NA` values. This is useful for gene list server
#' queries, for example.
#'
#' @rdname sortUnique
#'
#' @param vector Vector with duplicates, `NA` values.
#'
#' @return Unique vector.
#' @export
#'
#' @examples
#' sortUnique(c("milk", "eggs", "eggs", NA))
setMethod("sortUnique", "character", function(object) {
    object %>%
        na.omit %>%
        sort %>%
        unique
})
