#' Match interesting groups
#'
#' Match the user-defined interesting groups to the values slotted within a
#' `SummarizedExperiment` object, and check that they are valid. Otherwise
#' supports a missing `interestingGroups` argument, which will then use the
#' `interestingGroups` accessor function internally.
#'
#' @export
#'
#' @inheritParams params
#' @param object `SummarizedExperiment`.
#'
#' @return `character`.
#' Interesting groups.
#'
#' @examples
#' data(rse)
#' matchInterestingGroups(rse)
#'
#' matchInterestingGroups(rse, interestingGroups = NULL)
#' matchInterestingGroups(rse, interestingGroups = substitute())
matchInterestingGroups <- function(object, interestingGroups = NULL) {
    # Legacy support for bcbio R packages, which pass missing
    # `interestingGroups` parameter through.
    if (
        missing(interestingGroups) ||
        !is.character(interestingGroups)
    ) {
        interestingGroups <- NULL
    }

    if (is.null(interestingGroups)) {
        interestingGroups <- interestingGroups(object)
    }

    # Attempt to assign and return, if user is passing in a character vector.
    if (is.character(interestingGroups)) {
        interestingGroups <- tryCatch(
            expr = {
                interestingGroups(object) <- interestingGroups
                interestingGroups(object)
            },
            error = function(e) NULL
        )
    }

    # Return `sampleName` by default, if necessary.
    if (is.null(interestingGroups)) {
        interestingGroups <- "sampleName"
    }

    assert(isCharacter(interestingGroups))
    interestingGroups
}
