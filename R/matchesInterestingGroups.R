#' Check that interesting groups match a defined value
#'
#' Prevent unwanted downstream behavior when a missing interesting group
#' is requested by the user.
#'
#' @note Updated 2019-08-11.
#' @export
#'
#' @inherit goalie::check return
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## Checks that columns are defined in `sampleData()`.
#' matchesInterestingGroups(rse, "condition")
#'
#' ## Currently allowing `NULL` to pass.
#' matchesInterestingGroups(rse, NULL)
matchesInterestingGroups <- function(
    x,
    interestingGroups,
    .xname = getNameInParent(x)
) {
    ok <- isS4(x)
    if (!isTRUE(ok)) {
        return(false("'%s' is not S4 class.", .xname))
    }

    ## Early return on `NULL` interesting groups (e.g. example DESeqDataSet).
    ## Consider returning FALSE or warning in a future update?
    if (is.null(interestingGroups)) {
        return(TRUE)
    }

    ## Otherwise, require a character vector.
    ok <- isCharacter(interestingGroups)
    if (!isTRUE(ok)) return(ok)

    ## Using `sampleData()` to check against `interestingGroups` column.
    data <- sampleData(x)

    ## Check intersection with sample data.
    ok <- isSubset(interestingGroups, colnames(data))
    if (!isTRUE(ok)) {
        return(false("Interesting groups are not defined in 'sampleData()'."))
    }

    ## Check that interesting groups columns are factors.
    ok <- all(vapply(
        X = data[, interestingGroups, drop = FALSE],
        FUN = is.factor,
        FUN.VALUE = logical(1L)
    ))
    if (!isTRUE(ok)) {
        return(false("Interesting group columns are not all factor."))
    }

    TRUE
}
