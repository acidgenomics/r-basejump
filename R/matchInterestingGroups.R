#' Match Interesting Groups
#'
#' Match the user-defined interesting groups to the values slotted within a
#' `SummarizedExperiment` object, and check that they are valid. Otherwise
#' supports a missing `interestingGroups` argument, which will then use the
#' `interestingGroups()` accessor function internally.
#'
#' @export
#'
#' @inheritParams params
#' @param object `SummarizedExperiment`.
#'
#' @return `character`. Interesting groups.
#'
#' @examples
#' data(rse)
#' matchInterestingGroups(rse)
matchInterestingGroups <- function(object, interestingGroups = NULL) {
    if (is.null(interestingGroups)) {
        interestingGroups <- interestingGroups(object, check = FALSE)
        if (
            is.null(interestingGroups) ||
            !all(interestingGroups %in% colnames(colData(object)))
        ) {
            interestingGroups <- "sampleName"
        }
    } else {
        interestingGroups(object) <- interestingGroups
    }
    assert(isCharacter(interestingGroups))
    interestingGroups
}



#' Check Interesting Groups Value Matching
#'
#' Prevent unwanted downstream behavior when a missing interesting group
#' is requested by the user.
#'
#' @inherit params
#' @export
#'
#' @param x S4 class x.
#' @param interestingGroups `character`. Interesting groups.
#'
#' @examples
#' data(rse, package = "basejump")
#' matchesInterestingGroups(rse, "condition")
#' matchesInterestingGroups(rse, NULL)
matchesInterestingGroups <- function(x, interestingGroups) {
    assert(isS4(x))
    data <- sampleData(x)

    # Check `interestingGroups` argument.
    if (is.null(interestingGroups)) {
        # Early return clean on `NULL` value (e.g. DESeqDataSet).
        return(invisible())
    } else {
        # Otherwise, require that `interestingGroups` is a character.
        assert(is.character(interestingGroups))
    }

    # Check intersection with sample data.
    assert(isSubset(interestingGroups, colnames(data)))

    # Check that interesting groups columns are factors.
    assert(all(vapply(
        X = data[, interestingGroups, drop = FALSE],
        FUN = is.factor,
        FUN.VALUE = logical(1L)
    )))

    TRUE
}
