# FIXME Attempt to improve this error message to make it more informative.
# test_seurat_sce.R:72: failure: interestingGroups<-
#     `interestingGroups(object) <- "orig.ident"` threw an error with unexpected message.
# Expected match: "sampleData"
# Actual message: "is_subset : The element 'orig.ident' in interestingGroups is not in colnames(data)."



#' @name interestingGroups
#' @inheritParams params
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
        assertFormalInterestingGroups(x = object, interestingGroups = value)
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
