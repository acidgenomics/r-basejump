## FIXME MOVE TO ACIDEXPERIMENT.



#' @name headtail
#' @inherit AcidGenerics::headtail
#' @note Updated 2020-10-07.
#'
#' @inheritParams AcidRoxygen::params
#' @param n `integer(1)`.
#'   Positive integer denoting the number of first and last items to include.
#' @param ... Additional arguments.
#'
#' @examples
#' data(mtcars, package = "datasets")
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' headtail(rse)
NULL



## Updated 2020-10-07.
`headtail,matrix` <-  # nolint
    methodFunction(
        f = "headtail",
        signature = "matrix",
        package = "AcidBase"
    )



## Updated 2020-10-07.
`headtail,DataFrame` <-  # nolint
    getMethod(
        f = "headtail",
        signature = "data.frame",
        where = asNamespace("AcidBase")
    )



#' @describeIn headtail Same method as `data.frame`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("DataFrame"),
    definition = `headtail,DataFrame`
)



## Updated 2020-10-07.
`headtail,Matrix` <-  # nolint
    `headtail,matrix`



#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("Matrix"),
    definition = `headtail,Matrix`
)



## Updated 2020-05-11.
`headtail,GRanges` <-  # nolint
    function() {
        headtail(x = as(x, "data.frame"), n = n)
    }

formals(`headtail,GRanges`) <- formals(`headtail,matrix`)



#' @describeIn headtail Summarize the ranges.
#' @export
setMethod(
    f = "headtail",
    signature = signature("GRanges"),
    definition = `headtail,GRanges`
)



## Updated 2020-05-11.
`headtail,SummarizedExperiment` <-  # nolint
    function() {
        headtail(x = assay(x), n = n)
    }

formals(`headtail,SummarizedExperiment`) <- formals(`headtail,matrix`)



#' @describeIn headtail Summarize the primary `assay`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("SummarizedExperiment"),
    definition = `headtail,SummarizedExperiment`
)
