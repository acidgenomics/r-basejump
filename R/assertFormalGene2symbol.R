#' Gene to Symbol Mappings Formal Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @param x Object containing gene identifiers in the rownames.
#' @param genes Character vector of gene identifiers.
#' @param gene2symbol Gene-to-symbol mappings data frame.
#'
#' @export
assertFormalGene2symbol <- function(
    x,
    genes,
    gene2symbol,
    severity = "stop") {
    assert_is_any_of(genes, c("character", "NULL"))
    if (is.character(genes)) {
        assert_is_subset(genes, rownames(x))
    }
    assert_is_any_of(gene2symbol, c("data.frame", "NULL"))
    if (is.data.frame(gene2symbol)) {
        assert_is_gene2symbol(gene2symbol)
        assert_is_subset(rownames(x), rownames(gene2symbol))
    }
}



# Aliases ======================================================================
#' @rdname assertFormalGene2symbol
#' @export
assertFormalGene2symbol -> assert_formal_gene2symbol
