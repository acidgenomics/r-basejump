#' All generic functions
#' @include AllGenerics.R
#' @noRd
NULL



#' @rdname integerCounts
#' @export
setGeneric(
    name = "integerCounts",
    def = function(object, ...) {
        standardGeneric("integerCounts")
    }
)



#' @rdname makeSampleData
#' @export
setGeneric(
    name = "makeSampleData",
    def = function(object, ...) {
        standardGeneric("makeSampleData")
    }
)



#' @rdname makeSingleCellExperiment
#' @export
setGeneric(
    name = "makeSingleCellExperiment",
    def = function(assays, ...) {
        standardGeneric("makeSingleCellExperiment")
    }
)



#' @rdname makeSummarizedExperiment
#' @export
setGeneric(
    name = "makeSummarizedExperiment",
    def = function(assays, ...) {
        standardGeneric("makeSummarizedExperiment")
    }
)



#' @rdname headtail
#' @name headtail
#' @importFrom AcidGenerics headtail
#' @usage headtail(x, ...)
#' @export
NULL

#' @rdname melt
#' @name melt
#' @importFrom AcidGenerics melt
#' @usage melt(object, ...)
#' @export
NULL
