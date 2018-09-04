#' Sanitize Row Data
#'
#' Coerce gene annotations to a `tibble`, and keep only `atomic` columns.
#' Complex columns (e.g. Entrez ID `list`) will fail to write to disk as CSVs.
#'
#' Supports `GRanges` and `DataFrame` class objects.
#'
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `tbl_df`, containing only `atomic` columns.
#' @export
#'
#' @examples
#' from <- SummarizedExperiment::rowRanges(rse_small)
#' colnames(S4Vectors::mcols(from))
#' to <- sanitizeRowData(from)
#' vapply(to, is.atomic, logical(1L))
#' print(to)
sanitizeRowData <- function(object) {
    assert_is_any_of(
        x = object,
        classes = c("GRanges", "DataFrame")
    )
    validObject(object)

    # Coerce to tibble.
    data <- as(object, "tbl_df")

    # Enforce camel case.
    data <- camel(data)

    # Keep only atomic columns. Complex columns won't write to disk as CSVs
    # or work with R Markdown functions.
    keep <- vapply(
        X = data,
        FUN = is.atomic,
        FUN.VALUE = logical(1L)
    )
    data <- data[, keep, drop = FALSE]
    assert_is_non_empty(data)

    data
}
