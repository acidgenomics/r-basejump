#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertSymbolsToGenes",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        g2s <- gene2symbol(object)
        if (is.null(g2s)) {
            stop("Object does not contain gene-to-symbol mappings")
        }
        genes <- g2s[, "geneID", drop = TRUE]
        assert_has_no_duplicates(genes)
        rownames(object) <- genes
        object
    }
)
