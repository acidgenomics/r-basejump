#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertSymbolsToGenes",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        g2s <- gene2symbol(object)
        if (is.null(g2s)) {
            # nocov start
            warning("Object does not contain gene-to-symbol mappings")
            return(object)
            # nocov end
        }
        genes <- g2s[, "geneID", drop = TRUE]
        rownames(object) <- genes
        object
    }
)
