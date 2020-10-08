#' @name interestingGroups
#' @inherit AcidGenerics::interestingGroups
#' @note Updated 2019-08-11.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' intgroup <- interestingGroups(rse)
#' print(intgroup)
#' ## Assignment support.
#' interestingGroups(rse) <- intgroup[[1L]]
#' interestingGroups(rse)
NULL



## Updated 2019-07-22.
`interestingGroups,Annotated` <-  # nolint
    function(object) {
        metadata(object)[["interestingGroups"]]
    }



#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups",
    signature = signature("Annotated"),
    definition = `interestingGroups,Annotated`
)



## Updated 2019-07-22.
`interestingGroups<-,Annotated,character` <-  # nolint
    function(object, value) {
        assert(areDisjointSets(value, "interestingGroups"))
        metadata(object)[["interestingGroups"]] <- value
        object
    }



#' @rdname interestingGroups
#' @export
setReplaceMethod(
    f = "interestingGroups",
    signature = signature(
        object = "Annotated",
        value = "character"
    ),
    definition = `interestingGroups<-,Annotated,character`
)



## Updated 2019-08-11.
`interestingGroups<-,SummarizedExperiment,character` <-  # nolint
    function(object, value) {
        ## Check for attempt to use `interestingGroups` automatic column.
        assert(areDisjointSets(value, "interestingGroups"))
        ## Note that we're always allowing `sampleName` to be slotted, even if
        ## that column isn't defined in `colData()`.
        setdiff <- setdiff(value, colnames(sampleData(object)))
        if (hasLength(setdiff)) {
            stop(sprintf(
                "Columns not defined in 'sampleData()': %s.",
                toString(setdiff, width = 100L)
            ))
        }
        metadata(object)[["interestingGroups"]] <- value
        object
    }



#' @rdname interestingGroups
#' @export
setReplaceMethod(
    f = "interestingGroups",
    signature = signature(
        object = "SummarizedExperiment",
        value = "character"
    ),
    definition = `interestingGroups<-,SummarizedExperiment,character`
)



## Updated 2019-07-22.
`interestingGroups<-,Annotated,NULL` <-  # nolint
    function(object, value) {
        metadata(object)[["interestingGroups"]] <- NULL
        object
    }



#' @rdname interestingGroups
#' @export
setReplaceMethod(
    f = "interestingGroups",
    signature = signature(
        object = "Annotated",
        value = "NULL"
    ),
    definition = `interestingGroups<-,Annotated,NULL`
)
