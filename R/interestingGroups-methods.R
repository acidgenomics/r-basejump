#' Interesting Groups
#'
#' @name interestingGroups
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `character`.
#'
#' @examples
#' # SummarizedExperiment ====
#' interestingGroups(rse_small)
#' colnames(colData(rse_small))
#' interestingGroups(rse_small) <- colnames(colData(rse_small))[[1L]]
#' interestingGroups(rse_small)
NULL



#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups",
    signature = signature("SummarizedExperiment"),
    definition = function(object) {
        value <- metadata(object)[["interestingGroups"]]
        assertFormalInterestingGroups(object, value)
        value
    }
)



#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "character"
    ),
    definition = function(object, value) {
        assertFormalInterestingGroups(object, value)
        metadata(object)[["interestingGroups"]] <- value
        object
    }
)
