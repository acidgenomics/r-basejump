#' @name nonzero
#' @inherit bioverbs::nonzero
#' @note Updated 2019-08-23.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' dim(object)
#' x <- nonzero(object)
#' dim(x)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' dim(object)
#' x <- nonzero(object)
#' dim(x)
NULL



#' @rdname nonzero
#' @name nonzero
#' @importFrom bioverbs nonzero
#' @usage nonzero(object, ...)
#' @export
NULL



## FIXME matrix
`nonzero,matrix` <-  # nolint
    function(object) {
    }



## FIXME Matrix



## FIXME DelayedArray



## Updated 2019-08-23.
`nonzero,SummarizedExperiment` <-  # nolint
    function(object) {
        ## FIXME
        stop("FIXME")
    }



#' @rdname nonzero
#' @export
setMethod(
    f = "nonzero",
    signature = signature("SummarizedExperiment"),
    definition = `nonzero,SummarizedExperiment`
)
