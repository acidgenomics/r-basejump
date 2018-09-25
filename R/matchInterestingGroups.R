#' Match Interesting Groups
#'
#' Match the user-defined interesting groups to the values slotted within a
#' `SummarizedExperiment` object, and check that they are valid. Otherwise
#' supports a missing `interestingGroups` argument, which will then use the
#' [interestingGroups()] accessor function internally.
#'
#' @author Michael Steinbaugh
#' @family Developer Functions
#' @export
#'
#' @inheritParams general
#' @param object `SummarizedExperiment`.
#'
#' @return `character`. Interesting groups.
#'
#' @examples
#' matchInterestingGroups(rse_small)
matchInterestingGroups <- function(object, interestingGroups = NULL) {
    assert_is_all_of(object, "SummarizedExperiment")
    if (is.null(interestingGroups)) {
        interestingGroups <- basejump::interestingGroups(object)
        if (is.null(interestingGroups)) {
            interestingGroups <- "sampleName"
        }
    } else {
        interestingGroups(object) <- interestingGroups
    }
    assert_is_character(interestingGroups)
    assert_is_non_empty(interestingGroups)
    interestingGroups
}
