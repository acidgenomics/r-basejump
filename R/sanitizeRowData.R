# TODO Include `.sanitizeAnnotationCols()` call here?

#' Sanitize Row Data
#'
#' Coerce Ensembl `rowData` returned from the [genes()] or [transcripts()]
#' functions to a `data.frame`, and drop any nested list columns (e.g.
#' `entrez`). Nested columns will fail to write to disk as CSVs.
#'
#' Supports `GRanges`, `data.frame`, and `DataFrame` objects.
#'
#' @family Sanitization Functions
#'
#' @inheritParams general
#'
#' @return `data.frame`, without any nested list columns.
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
sanitizeRowData <- function(object) {
    assert_is_any_of(object, ensemblReturn)
    data <- as.data.frame(object)

    # Set the rownames, if they're missing
    if (!hasRownames(data)) {
        idCol <- .detectIDCol(data)
        rownames(data) <- data[[idCol]]
    }

    # Drop any nested list columns (e.g. `entrez`). These's don't play
    # nicely with downstream R Markdown functions.
    nestedCols <- vapply(
        X = data,
        FUN = is.list,
        FUN.VALUE = logical(1L)
    )
    if (any(nestedCols)) {
        data <- data[, which(!nestedCols), drop = FALSE]
    }

    data
}
