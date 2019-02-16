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
    # Legacy support for bcbio R packages.
    if (missing(interestingGroups)) {
        interestingGroups <- NULL
    }
    if (isCharacter(interestingGroups)) {
        interestingGroups(object) <- interestingGroups
    }
    # Note that `check = FALSE` must be set here.
    # Note that `matchesInterestingGroups()` is called inside
    # `interestingGroups()` method, so this code can get circular.
    interestingGroups <- interestingGroups(object, check = FALSE)
    if (
        is.null(interestingGroups) ||
        !all(interestingGroups %in% colnames(colData(object)))
    ) {
        interestingGroups <- "sampleName"
    }
    assert(isCharacter(interestingGroups))
    interestingGroups
}
