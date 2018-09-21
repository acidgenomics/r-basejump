#' Interesting Groups
#'
#' @name interestingGroups
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `character`.
#'
#' @examples
#' # SummarizedExperiment ====
#' object <- rse_small
#' interestingGroups(object)
#'
#' # Assignment support.
#' interestingGroups(object) <- "sampleName"
#' interestingGroups(object)
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
        assertFormalInterestingGroups(
            object = object,
            interestingGroups = value
        )
        metadata(object)[["interestingGroups"]] <- value
        # Ensure `interestingGroups` column is updated to match.
        sampleData(object) <- sampleData(object)
        object
    }
)
