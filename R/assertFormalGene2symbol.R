#' Assert Formal Gene to Symbol Mappings
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param x Object containing gene identifiers in the rownames.
#' @param genes Character vector of gene identifiers.
#' @param gene2symbol Gene-to-symbol mappings data frame.
#'
#' @export
#'
#' @examples
#' gene2symbol <- data.frame(
#'     "geneID" = c("ENSG00000000003", "ENSG00000000005"),
#'     "geneName" = c("TSPAN6", "TNMD"),
#'     row.names = c("ENSG00000000003", "ENSG00000000005")
#' )
#' genes <- head(rownames(gene2symbol), 2L)
#' x <- data.frame(
#'     "sample_1" = c(1L, 2L),
#'     "sample_2" = c(3L, 4L),
#'     row.names = genes,
#'     stringsAsFactors = FALSE
#' )
#' assertFormalGene2symbol(x, genes, gene2symbol)
assertFormalGene2symbol <- function(
    x,
    genes,
    gene2symbol,
    severity = getOption("assertive.severity", "stop")
) {
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
