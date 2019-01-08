#' Sanitize sample data
#' @export
#' @inheritParams params
#' @examples
#' data(rse)
#' from <- sampleData(rse)
#' print(from)
#' to <- sanitizeSampleData(from)
#' all(vapply(to, is.factor, logical(1L)))
#' print(to)
sanitizeSampleData <- function(object) {
    assert(
        # Require `sampleName` column.
        "sampleName" %in% colnames(object),
        # Check for any duplicate rows.
        hasNoDuplicates(object[["sampleName"]])
    )
    # Drop blacklisted columns.
    blacklist <- c("interestingGroups", "sampleID")
    object <- object[, setdiff(colnames(object), blacklist), drop = FALSE]
    # This will flatten the S4 columns if possible and drop non-atomic.
    object <- sanitizeColData(object)
    # Ensure all columns are factors, with up-to-date levels.
    object <- factorize(object)
    # Return.
    object
}
