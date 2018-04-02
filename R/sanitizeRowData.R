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
#' x <- makeGRangesFromEnsembl(
#'     organism = "Homo sapiens",
#'     format = "genes",
#'     genomeBuild = "GRCh37"
#' )
#' sanitizeRowData(x) %>% glimpse()
#'
#' # Transcripts ====
#' x <- makeGRangesFromEnsembl(
#'     organism = "Homo sapiens",
#'     format = "transcripts",
#'     genomeBuild = "GRCh37"
#' )
#' sanitizeRowData(x) %>% glimpse()
sanitizeRowData <- function(object) {
    assert_is_any_of(object, ensemblReturn)
    data <- as.data.frame(object)

    # Enforce camel case
    data <- camel(data)

    # Set the rownames, if they're missing
    if (!hasRownames(data)) {
        idCol <- .detectIDCol(data)
        rownames(data) <- data[[idCol]]
    }

    # Drop any nested list columns (e.g. `entrezID`). These's don't play
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
