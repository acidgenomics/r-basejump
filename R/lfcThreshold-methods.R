#' @name lfcThreshold
#' @inherit bioverbs::lfcThreshold
#' @param ... Additional arguments.
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## Annotated
#' lfcThreshold(rse) <- 0.5
#' lfcThreshold(rse)
NULL



#' @rdname lfcThreshold
#' @name lfcThreshold
#' @importFrom bioverbs lfcThreshold
#' @usage lfcThreshold(object, ...)
#' @export
NULL

#' @rdname lfcThreshold
#' @name lfcThreshold<-
#' @importFrom bioverbs lfcThreshold<-
#' @usage lfcThreshold(object, ...) <- value
#' @export
NULL



## Updated 2019-07-22.
`lfcThreshold,Annotated` <-  # nolint
    function(object) {
        validObject(object)
        metadata(object)[["lfcThreshold"]]
    }



#' @rdname lfcThreshold
#' @export
setMethod(
    f = "lfcThreshold",
    signature = signature("Annotated"),
    definition = `lfcThreshold,Annotated`
)



## Updated 2019-07-22.
`lfcThreshold<-,Annotated,numeric` <-  # nolint
    function(object, value) {
        assert(
            isScalar(value),
            isNonNegative(value)
        )
        metadata(object)[["lfcThreshold"]] <- value
        validObject(object)
        object
    }



#' @rdname lfcThreshold
#' @export
setMethod(
    f = "lfcThreshold<-",
    signature = signature(
        object = "Annotated",
        value = "numeric"
    ),
    definition = `lfcThreshold<-,Annotated,numeric`
)
