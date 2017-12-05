#' Sanitize Ensembl Annotations
#'
#' @inheritParams checkAnnotable
#'
#' @return Annotable without nested list items (e.g. Entrez IDs).
#' @export
#'
#' @examples
#' human <- annotable("Homo sapiens")
#' sanitizeAnnotable(human) %>% glimpse()
sanitizeAnnotable <- function(object) {
    # Drop any nested list columns (e.g. `entrez`). These's don't play
    # nicely with downstream R Markdown functions.
    nestedCols <- vapply(
        X = object,
        FUN = is.list,
        FUN.VALUE = logical(1))
    if (any(nestedCols)) {
        object <- object[, which(!nestedCols)]
    }
    object
}
