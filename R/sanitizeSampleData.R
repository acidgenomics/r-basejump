#' Sanitize sample data
#'
#' @section Blacklist:
#'
#' Here's the current column blacklist:
#'
#' - interestingGroups.
#' - sampleID.
#'
#' @export
#' @inheritParams params
#'
#' @return `DataFrame`.
#' Sanitized data frame containing only non-blacklisted columns and all
#' `character` columns coerced to `factor` (i.e. `stringsAsFactors`).
#'
#' @examples
#' data(rse)
#' from <- sampleData(rse)
#' print(from)
#' to <- sanitizeSampleData(from)
#' all(vapply(to, is.factor, logical(1L)))
#' print(to)
sanitizeSampleData <- function(object) {
    assert(
        is(object, "DataFrame"),
        # Require `sampleName` column.
        "sampleName" %in% colnames(object),
        # Check for any duplicate rows.
        hasNoDuplicates(object[["sampleName"]]),
        hasRownames(object)
    )
    # Drop blacklisted columns.
    blacklist <- c("interestingGroups", "sampleID")
    object <- object[, setdiff(colnames(object), blacklist), drop = FALSE]
    # This will flatten the S4 columns if possible and drop non-atomic.
    object <- atomize(object)
    # Ensure all columns are factors, with up-to-date levels.
    object <- factorize(object)
    assert(
        is(object, "DataFrame"),
        hasRownames(object)
    )
    # Return.
    object
}
