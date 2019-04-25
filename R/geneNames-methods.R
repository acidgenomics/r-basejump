#' @name geneNames
#' @inherit bioverbs::geneNames
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(rse, package = "acidtest")
#'
#' ## SummarizedExperiment ====
#' head(geneNames(rse))
NULL



geneNames.Vector <-  # nolint
    function(object) {
        Gene2Symbol(object, format = "makeUnique")[["geneName"]]
    }



#' @rdname geneNames
#' @export
setMethod(
    f = "geneNames",
    signature = signature("GRanges"),
    definition = geneNames.Vector
)



#' @rdname geneNames
#' @export
setMethod(
    f = "geneNames",
    signature = signature("SummarizedExperiment"),
    definition = geneNames.Vector
)
