## FIXME MOVE THIS TO ACIDSINGLECELL.



#' @name geometricMean
#' @inherit AcidGenerics::geometricMean
#'
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams base::apply
#' @param ... Additional arguments.
#'
#' @examples
#' ## Matrix ====
#' sparse <- as(matrix, "sparseMatrix")
#' print(sparse)
#' geometricMean(sparse)
NULL



## Updated 2021-02-02.
`geometricMean,Matrix` <-  # nolint
    methodFunction(
        f = "geometricMean",
        signature = "matrix",
        package = "AcidBase"
    )



#' @rdname geometricMean
#' @export
setMethod(
    f = "geometricMean",
    signature = signature("Matrix"),
    definition = `geometricMean,Matrix`
)
