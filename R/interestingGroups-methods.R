#' Interesting Groups
#'
#' @name interestingGroups
#' @inheritParams params
#'
#' @return `character`.
#'
#' @examples
#' data(rse)
#' object <- rse
#'
#' intgroup <- interestingGroups(object)
#' print(intgroup)
#'
#' ## Assignment support.
#' interestingGroups(object) <- intgroup[[1L]]
#' interestingGroups(object)
NULL



# Keep `check` disabled by default.
#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups",
    signature = signature("SummarizedExperiment"),
    definition = function(object, check = FALSE) {
        assert(isFlag(check))
        value <- metadata(object)[["interestingGroups"]]
        if (isTRUE(check)) {
            assert(matchesInterestingGroups(object, value))
        }
        value
    }
)



# We're always checking assignment validity here.
#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "character"
    ),
    definition = function(object, value) {
        assert(matchesInterestingGroups(object, value))
        metadata(object)[["interestingGroups"]] <- value
        validObject(object)
        object
    }
)



#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "NULL"
    ),
    definition = function(object, value) {
        metadata(object)[["interestingGroups"]] <- NULL
        validObject(object)
        object
    }
)
