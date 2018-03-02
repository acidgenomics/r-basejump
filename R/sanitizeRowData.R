#' Sanitize Row Data
#'
#' Coerce Ensembl `rowData` returned from the [genes()] or [transcripts()]
#' functions to a data frame, and drop any nested list columns (e.g. `entrez`),
#' if desired.
#'
#' Supports [GRanges], [data.frame], and [DataFrame] objects.
#'
#' @family Sanitization Functions
#'
#' @inheritParams general
#'
#' @param dropNested Drop any nested list columns (e.g. `entrez`). Otherwise, a
#'   user may run into issues attempting to write results to flat CSV files.
#'
#' @return Data frame, without nested list columns.
#' @export
#'
#' @examples
#' # Gene annotations
#' genes <- genes("Homo sapiens")
#' sanitizeRowData(genes) %>% glimpse()
#'
#' # Transcript annotations
#' transcripts <- transcripts("Homo sapiens")
#' sanitizeRowData(transcripts) %>% glimpse()
sanitizeRowData <- function(object, dropNested = TRUE) {
    assert_is_any_of(object, ensemblReturn)
    object <- as.data.frame(object)
    assert_is_a_bool(dropNested)

    # Set the rownames, if they're missing
    if (!hasRownames(object)) {
        idCol <- .detectIDCol(object)
        rownames(object) <- object[[idCol]]
    }

    # Drop any nested list columns (e.g. `entrez`). These's don't play
    # nicely with downstream R Markdown functions.
    if (isTRUE(dropNested)) {
        nestedCols <- vapply(
            X = object,
            FUN = is.list,
            FUN.VALUE = logical(1L))
        if (any(nestedCols)) {
            object <- object[, which(!nestedCols), drop = FALSE]
        }
    }

    object
}
