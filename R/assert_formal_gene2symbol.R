#' Formal `gene2symbol` Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @param object Object containing gene identifiers in the rownames.
#' @param genes Character vector of gene identifiers.
#' @param gene2symbol Gene-to-symbol mappings data frame.
#'
#' @export
assert_formal_gene2symbol <- function(object, genes, gene2symbol) {
    assert_is_any_of(genes, c("character", "NULL"))
    if (is.character(genes)) {
        assert_is_subset(genes, rownames(object))
    }
    assert_is_any_of(gene2symbol, c("data.frame", "NULL"))
    if (is.data.frame(gene2symbol)) {
        assert_is_gene2symbol(gene2symbol)
        assert_is_subset(rownames(object), rownames(gene2symbol))
    }
}
