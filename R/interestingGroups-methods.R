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
#' intgroup <- interestingGroups(object)
#' print(intgroup)
#'
#' # Assignment support.
#' interestingGroups(object) <- intgroup[[1L]]
#' interestingGroups(object)
NULL



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
    definition = getMethod(
        f = "interestingGroups<-",
        signature(
            object = "SummarizedExperiment",
            value = "character"
        )
    )
)
