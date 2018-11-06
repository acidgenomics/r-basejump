#' @name interestingGroups
#' @inherit basejump.generics::interestingGroups
#' @inheritParams params
#'
#' @examples
#' data(rse, package = "basejump.data")
#' object <- rse
#'
#' intgroup <- interestingGroups(object)
#' print(intgroup)
#'
#' ## Assignment support.
#' interestingGroups(object) <- intgroup[[1L]]
#' interestingGroups(object)
NULL



#' @importFrom basejump.generics interestingGroups
#' @aliases NULL
#' @export
basejump.generics::interestingGroups



#' @importFrom basejump.generics interestingGroups<-
#' @aliases NULL
#' @export
basejump.generics::`interestingGroups<-`



# interestingGroups ============================================================
#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups",
    signature = signature("SummarizedExperiment"),
    definition = function(object, check = TRUE) {
        assert_is_a_bool(check)
        value <- metadata(object)[["interestingGroups"]]
        if (isTRUE(check)) {
            assertFormalInterestingGroups(object, value)
        }
        value
    }
)



# interestingGroups assignment =================================================
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
        validObject(object)
        object
    }
)



# Started allowing `NULL` assignment in v0.99.
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
