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
#' sortUnique(c("milk", "eggs", "eggs", NA))
NULL



# Methods ======================================================================
#' @rdname sortUnique
#' @importFrom stats na.omit
#' @export
setMethod(
    "sortUnique",
    signature("character"),
    function(object) {
        object %>%
            na.omit() %>%
            sort() %>%
            unique()
    })
