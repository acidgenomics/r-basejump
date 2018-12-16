#' Sanitize column data
#'
#' Standardize data describing the colummns (e.g. samples or cells), keeping
#' only `atomic` columns.
#'
#' Always applied:
#'
#' - Non-`atomic` columns will be flattened to `atomic` if possible, and
#'   otherwise dropped.
#'
#' @section Sample data:
#'
#' The following conventions are enforced:
#'
#' - Required columns: `sampleName`.
#' - Blacklisted columns: `interestingGroups`, `sampleID`.
#' - All columns get coerced to `factor` and `droplevels` is applied.
#'
#' @param object `DataFrame`.
#' @export
#'
#' @return `DataFrame`.
#' Contains only `atomic` columns.
#'
#' @examples
#' data(rse)
#' from <- sampleData(rse)
#' print(from)
#' to <- sanitizeSampleData(from)
#' all(vapply(to, is.factor, logical(1L)))
#' print(to)
sanitizeColData <- function(object) {
    assert(
        is(object, "DataFrame"),
        isNonEmpty(object),
        hasRownames(object),
        hasColnames(object),
        hasValidDimnames(object)
    )
    .atomicDataFrame(object)
}



#' @rdname sanitizeColData
#' @export
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
    object <- .factorize(object)
    # Return.
    object
}



# Consider exporting this.
.atomicDataFrame <- function(object) {
    # First, coerce to S3 data frame.
    # This step helps coerce nested S4 data to atomic columns.
    # This will also decode Rle columns.
    object <- as.data.frame(object)
    # Keep only atomic columns. Complex columns won't write to disk as CSVs
    # or work with R Markdown functions.
    keep <- vapply(X = object, FUN = is.atomic, FUN.VALUE = logical(1L))
    object <- object[, keep, drop = FALSE]
    assert(hasLength(object))
    as(object, "DataFrame")
}



# Consider exporting this.
# See `encode` for Rle approach.
.factorize <- function(object) {
    out <- lapply(
        X = object,
        FUN = function(x) {
            droplevels(as.factor(x))
        }
    )
    out <- as(out, class(object)[[1L]])
    names(out) <- names(object)
    rownames <- rownames(object)
    if (!is.null(rownames)) {
        rownames(out) <- rownames
    }
    out
}
