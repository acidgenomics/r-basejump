#' Sanitize Row Data
#'
#' Coerce Ensembl `rowData` to a `data.frame`, and drop any nested list columns
#' (e.g. `entrezID`). Nested columns will fail to write to disk as CSVs.
#'
#' Supports `GRanges`, `DataFrame`, and `data.frame` class objects.
#'
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `data.frame`, without any nested `list` columns.
#' @export
#'
#' @examples
#' # Genes ====
#' x <- makeGRangesFromEnsembl("Homo sapiens", format = "genes")
#' sanitizeRowData(x) %>% glimpse()
#'
#' # Transcripts ====
#' x <- makeGRangesFromEnsembl("Homo sapiens", format = "transcripts")
#' sanitizeRowData(x) %>% glimpse()
sanitizeRowData <- function(object) {
    object <- as.data.frame(object)
    assertHasRownames(object)

    # Enforce camel case
    object <- camel(object)

    # Drop any nested list columns (e.g. `entrezID`). These's don't play
    # nicely with downstream R Markdown functions.
    nestedCols <- vapply(
        X = object,
        FUN = is.list,
        FUN.VALUE = logical(1L)
    )
    if (any(nestedCols)) {
        object <- object[, which(!nestedCols), drop = FALSE]
    }

    object
}
