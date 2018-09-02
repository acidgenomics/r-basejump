# Work on using match.call method here instead



#' Match Interesting Groups
#'
#' @author Michael Steinbaugh
#' @family Developer Functions
#'
#' @inheritParams general
#'
#' @return `character`.
#' @export
#'
#' @examples
#' matchInterestingGroups(rse_small)
#' matchInterestingGroups(rse_small, interestingGroups = NULL)
#' matchInterestingGroups(rse_small, interestingGroups = "sampleName")
matchInterestingGroups <- function(object, interestingGroups, ...) {
    if (missing(interestingGroups)) {
        interestingGroups <- basejump::interestingGroups(object)
        if (is.null(interestingGroups)) {
            interestingGroups <- "sampleName"  # nocov
        }
    } else if (is.null(interestingGroups)) {
        interestingGroups <- "sampleName"
    } else {
        interestingGroups(object) <- interestingGroups
    }
    assert_is_character(interestingGroups)
    assert_is_non_empty(interestingGroups)
    interestingGroups
}
