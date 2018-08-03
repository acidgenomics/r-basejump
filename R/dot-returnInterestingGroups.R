#' Return Interesting Groups
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @return `character`.
#' @export
#'
#' @examples
#' .returnInterestingGroups(rse_bcb)
.returnInterestingGroups <- function(
    object,
    interestingGroups
) {
    if (missing(interestingGroups)) {
        interestingGroups <- basejump::interestingGroups(object)
        if (is.null(interestingGroups)) {
            interestingGroups <- "sampleName"
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
