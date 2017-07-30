#' Convert Ensembl Identifier to Gene Symbol
#'
#' @rdname gene2symbol
#'
#' @return Same class as original object.
#' @export
#'
#' @examples
#' # character
#' gene2symbol(c("ENSMUSG00000000001", "ENSMUSG00000000003"))
setMethod("gene2symbol", "character", function(object) {
    organism <- detectOrganism(object[[1L]])
    g2s <- annotable(organism, format = "gene2symbol")
    if (!all(object %in% rownames(g2s))) {
        stop("Unknown transcripts present", call. = FALSE)
    }
    g2s[object, "symbol"]
})
