#' Quickly Perform Sort Unique on a Vector
#'
#' The function also strips `NA` values. This is useful for gene list server
#' queries, for example.
#'
#' @rdname sortUnique
#' @name sortUnique
#' @family Cleanup Utilities
#'
#' @inheritParams AllGenerics
#'
#' @return Unique vector.
#' @export
#'
#' @examples
#' sortUnique(c(NA, NA, "milk", "eggs", "eggs"))
NULL



# Constructors =================================================================
.sortUnique <- function(object) {
    object %>%
        sort(na.last = TRUE) %>%
        unique()
}



# Methods ======================================================================
#' @rdname sortUnique
#' @export
setMethod(
    "sortUnique",
    signature("factor"),
    .sortUnique)



#' @rdname sortUnique
#' @export
setMethod(
    "sortUnique",
    signature("vector"),
    .sortUnique)
