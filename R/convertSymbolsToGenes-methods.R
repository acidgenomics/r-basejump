#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertSymbolsToGenes",
    signature("SummarizedExperiment"),
    function(object) {
        object <- .coerceToSummarizedExperiment(object)
        gene2symbol <- gene2symbol(object)
        if (is.null(gene2symbol)) {
            return(object)  # nocov
        }
        genes <- gene2symbol %>%
            pull("geneID") %>%
            as.character()
        rownames(object) <- genes
        object
    }
)
