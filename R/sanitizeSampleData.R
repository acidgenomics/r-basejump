# TODO explain required and blacklisted columns more clearly.



#' Sanitize Sample Data
#'
#' This function will standardize sample metadata.
#'
#' The following conventions are enforced:
#'
#' - Column names will be converted to camel case, using [camel()].
#' - Data must contain a `sampleName` column.
#' - All columns will be coerced to factor.
#' - Non-atomic columns will be dropped.
#'
#' Currently supports `DataFrame` or `data.frame` input.
#'
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `DataFrame`.
#' @export
#'
#' @examples
#' object <- sampleData(rse_bcb)
#' x <- sanitizeSampleData(object)
#' all(vapply(x, is.factor, logical(1L)))
sanitizeSampleData <- function(object) {
    assert_is_any_of(object, c("DataFrame", "data.frame"))
    assert_is_non_empty(object)
    assert_has_colnames(object)

    # Require `sampleName` column.
    assert_is_subset(
        x = "sampleName",
        y = colnames(object)
    )

    # Error if blacklisted columns are detected.
    assert_are_disjoint_sets(
        x = colnames(object),
        y = c("interestingGroups", "sampleID")
    )

    # Error if non-atomic columns are detected.
    invisible(lapply(object, assert_is_atomic))

    # Ensure coercion to `DataFrame` class.
    data <- as(object, "DataFrame")
    assert_is_non_empty(data)
    data <- camel(data)
    assertHasRownames(data)

    # Require that dimnames are valid.
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

    data
}
