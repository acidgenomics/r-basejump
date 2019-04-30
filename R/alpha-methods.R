#' @name alpha
#' @inherit bioverbs::alpha
#' @param ... Additional arguments.
#' @examples
#' data(rse, package = "acidtest")
#'
#' ## Annotated
#' alpha(rse) <- 0.05
#' alpha(rse)
NULL



#' @rdname alpha
#' @name alpha
#' @importFrom bioverbs alpha
#' @usage alpha(object, ...)
#' @export
NULL

#' @rdname alpha
#' @name alpha<-
#' @importFrom bioverbs alpha<-
#' @usage mcolnames(object, ...) <- value
#' @export
NULL



alpha.Annotated <-  # nolint
    function(object) {
        validObject(object)
        metadata(object)[["alpha"]]
    }



#' @rdname alpha
#' @export
setMethod(
    f = "alpha",
    signature = signature("Annotated"),
    definition = alpha.Annotated
)



`alpha<-.Annotated,character` <-  # nolint
    function(object, value) {
        assert(isAlpha(value))
        metadata(object)[["alpha"]] <- value
        validObject(object)
        object
    }



#' @rdname alpha
#' @export
setMethod(
    f = "alpha<-",
    signature = signature(
        object = "Annotated",
        value = "character"
    ),
    definition = `alpha<-.Annotated,character`
)
