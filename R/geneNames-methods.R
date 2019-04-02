#' @name geneNames
#' @inherit bioverbs::geneNames
#' @inheritParams params
#' @examples
#' data(rse, package = "acidtest")
#'
#' ## SummarizedExperiment ====
#' head(geneNames(rse))
NULL



#' @rdname geneNames
#' @name geneNames
#' @importFrom bioverbs geneNames
#' @export
NULL



geneNames.ANY <-  # nolint
    function(object) {
        Gene2Symbol(object, format = "makeUnique")[["geneName"]]
    }



#' @rdname geneNames
#' @export
setMethod(
    f = "geneNames",
    signature = signature("GRanges"),
    definition = geneNames.ANY
)



#' @rdname geneNames
#' @export
setMethod(
    f = "geneNames",
    signature = signature("SummarizedExperiment"),
    definition = geneNames.ANY
)
