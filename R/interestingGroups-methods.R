#' @name interestingGroups
#' @inherit bioverbs::interestingGroups
#' @inheritParams params
#' @examples
#' data(rse, package = "acidtest")
#' object <- rse
#'
#' intgroup <- interestingGroups(object)
#' print(intgroup)
#'
#' ## Assignment support.
#' interestingGroups(object) <- intgroup[[1L]]
#' interestingGroups(object)
NULL



#' @rdname interestingGroups
#' @name interestingGroups
#' @importFrom bioverbs interestingGroups
#' @export
NULL

#' @rdname interestingGroups
#' @name interestingGroups<-
#' @importFrom bioverbs interestingGroups<-
#' @export
NULL



interestingGroups.Annotated <-  # nolint
    function(object) {
        metadata(object)[["interestingGroups"]]
    }



#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups",
    signature = signature("Annotated"),
    definition = interestingGroups.Annotated
)



`interestingGroups<-.Annotated,character` <-  # nolint
    function(object, value) {
        assert(areDisjointSets(value, "interestingGroups"))
        metadata(object)[["interestingGroups"]] <- value
        object
    }



#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups<-",
    signature = signature(
        object = "Annotated",
        value = "character"
    ),
    definition = `interestingGroups<-.Annotated,character`
)



`interestingGroups<-.SummarizedExperiment,character` <-  # nolint
    function(object, value) {
        # Check for attempt to use `interestingGroups` automatic column.
        assert(areDisjointSets(value, "interestingGroups"))
        # Note that we're always allowing `sampleName` to be slotted, even if
        # that column isn't defined in `colData()`.
        if (
            !isSubset(value, colnames(sampleData(object))) &&
            !identical(value, "sampleName")
        ) {
            stop("Interesting groups must be columns in `sampleData()`.")
        }
        metadata(object)[["interestingGroups"]] <- value
        object
    }



#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "character"
    ),
    definition = `interestingGroups<-.SummarizedExperiment,character`
)



`interestingGroups<-.Annotated,NULL` <-  # nolint
    function(object, value) {
        metadata(object)[["interestingGroups"]] <- NULL
        object
    }



#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups<-",
    signature = signature(
        object = "Annotated",
        value = "NULL"
    ),
    definition = `interestingGroups<-.Annotated,NULL`
)
