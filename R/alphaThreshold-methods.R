#' @name alphaThreshold
#' @inherit bioverbs::alphaThreshold
#' @note Updated 2019-07-28.
#' @param ... Additional arguments.
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## Annotated ====
#' alphaThreshold(rse) <- 0.05
#' alphaThreshold(rse)
NULL



#' @rdname alphaThreshold
#' @name alphaThreshold
#' @importFrom bioverbs alphaThreshold
#' @usage alphaThreshold(object, ...)
#' @export
NULL

#' @rdname alphaThreshold
#' @name alphaThreshold<-
#' @importFrom bioverbs alphaThreshold<-
#' @usage alphaThreshold(object, ...) <- value
#' @export
NULL



## Updated 2019-07-22.
`alphaThreshold,Annotated` <-  # nolint
    function(object) {
        validObject(object)
        metadata(object)[["alpha"]]
    }



#' @rdname alphaThreshold
#' @export
setMethod(
    f = "alphaThreshold",
    signature = signature("Annotated"),
    definition = `alphaThreshold,Annotated`
)



## Updated 2019-07-22.
`alphaThreshold<-,Annotated,numeric` <-  # nolint
    function(object, value) {
        assert(isAlpha(value))
        metadata(object)[["alpha"]] <- value
        validObject(object)
        object
    }



#' @rdname alphaThreshold
#' @export
setMethod(
    f = "alphaThreshold<-",
    signature = signature(
        object = "Annotated",
        value = "numeric"
    ),
    definition = `alphaThreshold<-,Annotated,numeric`
)
