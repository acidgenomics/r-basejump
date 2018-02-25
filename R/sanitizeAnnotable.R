#' Sanitize Ensembl Annotations
#'
#' Drops any nested list columns (e.g. `entrez`) that prevents an annotable
#' data frame from being written to disk as a CSV.
#'
#' @family Sanitization Functions
#'
#' @inheritParams general
#'
#' @return Data frame without nested list columns.
#' @export
#'
#' @examples
#' human <- annotable("Homo sapiens")
#' sanitizeAnnotable(human) %>% glimpse()
sanitizeAnnotable <- function(object) {
    assert_is_data.frame(object)
    assert_is_subset(geneAnnotationCols, colnames(object))
    # Drop any nested list columns (e.g. `entrez`). These's don't play
    # nicely with downstream R Markdown functions.
    nestedCols <- vapply(
        X = object,
        FUN = is.list,
        FUN.VALUE = logical(1L))
    if (any(nestedCols)) {
        object <- object[, which(!nestedCols)]
    }
    object
}
