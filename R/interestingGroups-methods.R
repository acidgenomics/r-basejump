#' @name interestingGroups
#' @inherit bioverbs::interestingGroups
#' @inheritParams params
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



#' @importFrom bioverbs interestingGroups
#' @aliases NULL
#' @export
bioverbs::interestingGroups

#' @importFrom bioverbs interestingGroups<-
#' @aliases NULL
#' @export
bioverbs::`interestingGroups<-`



#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups",
    signature = signature("SummarizedExperiment"),
    definition = function(object) {
        metadata(object)[["interestingGroups"]]
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
        if (!isSubset(value, colnames(colData(object)))) {
            stop(
                "Interesting groups must be columns in `colData()`.",
                call. = FALSE
            )
        }
        metadata(object)[["interestingGroups"]] <- value
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
        object
    }
)
