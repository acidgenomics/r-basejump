#' Sanitize sample data
#'
#' @section Blacklist:
#'
#' Here's the current column blacklist:
#'
#' - interestingGroups.
#' - sampleId.
#'
#' @note Updated 2021-01-14.
#' @export
#'
#' @param object `DataFrame` (recommended) or `data.frame` (legacy).
#'   Note that legacy `data.frame` support will be removed in a future update.
#'
#' @return `DataFrame`.
#' Sanitized data frame containing only non-blacklisted columns and all
#' `character` columns coerced to `factor` (i.e. `stringsAsFactors`).
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' from <- sampleData(rse)
#' print(from)
#' to <- sanitizeSampleData(from)
#' all(vapply(to, is.factor, logical(1L)))
#' print(to)
sanitizeSampleData <- function(object) {
    ## Still allowing standard `data.frame`, to support bcbioRNASeq v0.2.9.
    ## Require stricter `DataFrame` input in a future update.
    if (is.data.frame(object)) {
        legacy <- TRUE
        class <- class(object)[[1L]]
        object <- as(object, "DataFrame")
    } else {
        legacy <- FALSE
    }
    assert(
        is(object, "DataFrame"),
        ## Require `sampleName` column.
        "sampleName" %in% colnames(object),
        ## Check for any duplicate rows.
        hasNoDuplicates(object[["sampleName"]]),
        hasRownames(object)
    )
    ## Drop blacklisted columns.
    blacklist <- c("interestingGroups", "sampleId")
    object <- object[, setdiff(colnames(object), blacklist), drop = FALSE]
    ## This will flatten the S4 columns if possible and drop non-atomic.
    object <- atomize(object)
    ## Ensure all columns are factors, with up-to-date levels.
    object <- factorize(object)
    assert(
        is(object, "DataFrame"),
        hasRownames(object)
    )
    ## Remove this step in a future update.
    if (isTRUE(legacy)) {
        object <- as(object, class)
    }
    ## Return.
    object
}
