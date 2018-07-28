#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertSymbolsToGenes",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        gene2symbol <- gene2symbol(object)
        if (is.null(gene2symbol)) {
            return(object)
        }
        genes <- gene2symbol %>%
            .[, "geneID", drop = TRUE] %>%
            as.character()
        rownames(object) <- genes
        object
    }
)
