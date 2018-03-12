# nocov start

#' @rdname deprecated
#' @export
setGeneric("annotable", function(object, ...) {
    standardGeneric("annotable")
})

#' @rdname deprecated
#' @export
setMethod(
    "annotable",
    signature("character"),
    function(object, ...) {
        .Deprecated("ensembl")
        ensembl(organism = object, ...)
    }
)

#' @rdname deprecated
#' @export
setMethod(
    "annotable",
    signature("data.frame"),
    function(object) {
        .Defunct()
    }
)

# nocov end
