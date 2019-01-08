#' @name geneNames
#' @inherit bioverbs::geneNames
#' @inheritParams params
#' @examples
#' data(rse)
#'
#' ## SummarizedExperiment ====
#' head(geneNames(rse))
NULL



#' @importFrom bioverbs geneNames
#' @aliases NULL
#' @export
bioverbs::geneNames



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
