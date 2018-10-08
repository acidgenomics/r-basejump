#' Sanitize Row Data
#'
#' Coerce gene annotations to `DataFrame`, and keep only `atomic` columns.
#' Complex columns (e.g. Entrez ID `list`) will fail to write to disk as CSVs.
#'
#' @note Supports `GRanges` and `DataFrame` class objects.
#'
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `DataFrame`. Contains only `character` and `factor` columns.
#'
#' @examples
#' data(rse_small)
#' from <- SummarizedExperiment::rowRanges(rse_small)
#' colnames(S4Vectors::mcols(from))
#' to <- sanitizeRowData(from)
#' vapply(to, is.atomic, logical(1L))
#' print(to)
sanitizeRowData <- function(object) {
    assert_is_any_of(object, classes = c("GRanges", "DataFrame"))
    validObject(object)

    # Coerce to tibble. We'll return as `DataFrame`.
    # This step helps coerce nested S4 data to atomic columns.
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

    # Return
    as(data, "DataFrame")
}
