#' Match interesting groups
#'
#' Match the user-defined interesting groups to the values slotted within a
#' `SummarizedExperiment` object, and check that they are valid. Otherwise
#' supports a missing `interestingGroups` argument, which will then use the
#' `interestingGroups` accessor function internally.
#'
#' @note Updated 2019-07-28.
#' @export
#'
#' @inheritParams AcidRoxygen::params
#' @param object `SummarizedExperiment`.
#'
#' @return `character`.
#' Interesting groups.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#' matchInterestingGroups(rse)
#' matchInterestingGroups(rse, interestingGroups = NULL)
#' matchInterestingGroups(rse, interestingGroups = substitute())
matchInterestingGroups <- function(object, interestingGroups = NULL) {
    ## Legacy support for bcbio R packages, which pass missing
    ## `interestingGroups` parameter through.
    if (
        missing(interestingGroups) ||
        !is.character(interestingGroups)
    ) {
        interestingGroups <- NULL
    }

    if (is.null(interestingGroups)) {
        interestingGroups <- interestingGroups(object)
    }

    ## Check that this metadata is defined in `colData()`.
    ## Don't check against `sampleData()` return because this function is used
    ## inside that code. Note that `sampleName` is a magic column that is
    ## automatically generated, if necessary.
    if (!is.null(interestingGroups)) {
        assert(
            areDisjointSets(
                x = interestingGroups,
                y = "interestingGroups"
            ),
            isSubset(
                x = interestingGroups,
                y = c("sampleName", colnames(colData(object)))
            )
        )
    }

    ## Return `sampleName` by default, if necessary.
    if (is.null(interestingGroups)) {
        interestingGroups <- "sampleName"
    }

    assert(isCharacter(interestingGroups))
    interestingGroups
}
