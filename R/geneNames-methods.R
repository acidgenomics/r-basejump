#' Gene Names
#'
#' Convenience function for quickly returning unique gene symbols that map to
#' the stable gene IDs that are defined in the row names.
#'
#' @name geneNames
#' @inheritParams params
#'
#' @return `character`.
#'
#' @examples
#' data(rse)
#'
#' ## SummarizedExperiment ====
#' head(geneNames(rse))
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
