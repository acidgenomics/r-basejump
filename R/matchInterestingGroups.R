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
#' matchInterestingGroups(rse_bcb, interestingGroups = "sampleName")
#' matchInterestingGroups(rse_bcb, interestingGroups = NULL)
matchInterestingGroups <- function(object, ...) {
    call <- match.call()
    interestingGroups <- as.character(call[["interestingGroups"]])
    if (!length(interestingGroups)) {
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
