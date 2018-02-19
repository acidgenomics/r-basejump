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
#'
#' @examples
#' gene2symbol <- gene2symbol("Homo sapiens", release = 90L)
#' genes <- head(rownames(gene2symbol), 2L)
#' x <- data.frame(
#'     sample1 = c(1L, 2L),
#'     saple2 = c(3L, 4L),
#'     row.names = genes,
#'     stringsAsFactors = FALSE)
#' assertFormalGene2symbol(x, genes, gene2symbol)
assertFormalGene2symbol <- function(
    x,
    genes,
    gene2symbol,
    severity = "stop") {
    assertHasRownames(x)
    assert_is_any_of(genes, c("character", "NULL"))
    if (is.character(genes)) {
        assert_is_subset(genes, rownames(x))
    }
    assert_is_any_of(gene2symbol, c("data.frame", "NULL"))
    if (is.data.frame(gene2symbol)) {
        assertIsGene2symbol(gene2symbol)
        assert_is_subset(rownames(x), rownames(gene2symbol))
    }
}
