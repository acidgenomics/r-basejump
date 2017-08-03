#' Quickly Perform Sort Unique on a Vector
#'
#' The function also strips `NA` values. This is useful for gene list server
#' queries, for example.
#'
#' @rdname sortUnique
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
