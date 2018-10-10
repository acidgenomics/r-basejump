#' Sanitize Sample Data
#'
#' This function will standardize sample metadata.
#'
#' The following conventions are enforced:
#'
#' - Column names will be converted to camel case, using [camel()].
#' - Required columns: `sampleName`.
#' - Blacklisted columns: `interestingGroups`, `sampleID`. These columns are
#'   generated internally, and should not be user defined.
#' - All columns will be coerced to factor.
#' - Non-atomic columns will be dropped.
#'
#' Currently supports `DataFrame` or `data.frame` input.
#'
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `DataFrame`.
#'
#' @examples
#' data(rse_small)
#' from <- sampleData(rse_small)
#' print(from)
#' to <- sanitizeSampleData(from)
#' all(vapply(to, is.factor, logical(1L)))
#' print(to)
sanitizeSampleData <- function(object) {
    assert_is_any_of(object, c("DataFrame", "data.frame"))
    assert_is_non_empty(object)
    assert_has_colnames(object)

    # Require `sampleName` column.
    assert_is_subset(
        x = "sampleName",
        y = colnames(object)
    )
    # And check for any duplicate rows.
    assert_has_no_duplicates(object[["sampleName"]])

    # Error if any non-atomic columns are detected.
    invisible(lapply(object, assert_is_atomic))

    # Coerce to DataFrame class.
    data <- as(object, "DataFrame")
    assert_is_non_empty(data)
    assert_has_rows(data)
    assertHasRownames(data)

    # Require that dimnames are valid.
    # This checks to ensure we don't have duplicate row or column names too.
    assertHasValidDimnames(data)

    # Coerce all columns to factor, and ensure levels are updated, in case
    # samples have been subset.
    rownames <- rownames(data)
    list <- lapply(
        X = data,
        FUN = function(x) {
            droplevels(as.factor(x))
        }
    )
    data <- as(list, "DataFrame")
    rownames(data) <- rownames

    # Sanitize to camel case.
    data <- camel(data)

    # Drop any blacklisted columns before return.
    blacklist <- c("interestingGroups", "sampleID")
    data <- data[, setdiff(colnames(data), blacklist), drop = FALSE]

    data
}
