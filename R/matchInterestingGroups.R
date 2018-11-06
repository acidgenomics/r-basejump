#' Match Interesting Groups
#'
#' Match the user-defined interesting groups to the values slotted within a
#' `SummarizedExperiment` object, and check that they are valid. Otherwise
#' supports a missing `interestingGroups` argument, which will then use the
#' [interestingGroups()] accessor function internally.
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
    assert_is_character(interestingGroups)
    assert_is_non_empty(interestingGroups)
    interestingGroups
}
