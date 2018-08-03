# Work on using match.call method here instead



#' Match Interesting Groups
#'
#' @author Michael Steinbaugh
#' @family Developer Functions
#'
#' @return `character`.
#' @export
#'
#' @examples
#' matchInterestingGroups(rse_bcb)
#' matchInterestingGroups(rse_bcb, interestingGroups = NULL)
#' matchInterestingGroups(rse_bcb, interestingGroups = "sampleName")
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
