#' @name uniteInterestingGroups
#' @inherit AcidGenerics::uniteInterestingGroups
#' @note Updated 2019-08-29.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## DataFrame ====
#' object <- rse
#' x <- uniteInterestingGroups(
#'     object = sampleData(object),
#'     interestingGroups = interestingGroups(object)
#' )
#' print(x)
NULL



## Updated 2019-07-22.
`uniteInterestingGroups,DataFrame` <-  # nolint
    function(object, interestingGroups) {
        assert(
            isCharacter(interestingGroups),
            isSubset(interestingGroups, colnames(object))
        )
        if (isScalar(interestingGroups)) {
            ## This will retain the factor levels, if they're not alphabetical.
            value <- object[[interestingGroups]]
        } else {
            ## Subset to get only the columns of interest.
            data <- object[, interestingGroups, drop = FALSE]
            ## This approach will return numerics for `DataFrame` class, so
            ## coercing columns to data.frame.
            value <- apply(
                X = as.data.frame(data),
                MARGIN = 1L,
                FUN = paste,
                collapse = ":"
            )
            value <- as.factor(value)
            assert(identical(rownames(object), names(value)))
        }
        object[["interestingGroups"]] <- unname(value)
        object
    }



#' @rdname uniteInterestingGroups
#' @export
setMethod(
    f = "uniteInterestingGroups",
    signature = signature("DataFrame"),
    definition = `uniteInterestingGroups,DataFrame`
)



## Deprecated legacy method support for bcbio R packages.
## Safe to deprecate/remove once bcbio v0.3 release series is on hbc.
`uniteInterestingGroups,data.frame` <-  # nolint
    `uniteInterestingGroups,DataFrame`



#' @rdname deprecated
#' @usage NULL
#' @export
setMethod(
    f = "uniteInterestingGroups",
    signature = signature("data.frame"),
    definition = `uniteInterestingGroups,data.frame`
)
