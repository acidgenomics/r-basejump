#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertSymbolsToGenes",
    signature = signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        g2s <- gene2symbol(object)
        if (is.null(g2s)) {
            stop("Object does not contain gene-to-symbol mappings")
        }
        assert_are_identical(rownames(object), g2s[["geneName"]])
        assert_has_no_duplicates(g2s[["geneID"]])
        rownames(object) <- g2s[["geneID"]]
        object
    }
)
