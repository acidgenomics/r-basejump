#' Check Gene-to-Symbol Value Matching
#'
#' @name matchesGene2Symbol
#' @inherit params
#'
#' @param x Object class supporting `rownames`. All rownames in this object
#'   must intersect with the rownames defined in the `gene2symbol` argument.
#'
#' @examples
#' DataFrame <- S4Vectors::DataFrame
#'
#' x <- DataFrame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(x)
#'
#' gene2symbol <- Gene2Symbol(
#'     object = DataFrame(
#'         geneID = c("ENSG00000000003", "ENSG00000000005"),
#'         geneName = c("TSPAN6", "TNMD"),
#'         row.names = rownames(x)
#'     )
#' )
#' print(gene2symbol)
#'
#' geneIDs <- gene2symbol[["geneID"]]
#' print(geneIDs)
#'
#' geneNames <- gene2symbol[["geneName"]]
#' print(geneNames)
#'
#' matchesGene2Symbol(
#'     x = x,
#'     genes = geneIDs,
#'     gene2symbol = gene2symbol
#' )
#' matchesGene2Symbol(
#'     x = x,
#'     genes = geneNames,
#'     gene2symbol = gene2symbol
#' )
NULL



.matchesGene2Symbol <- function(x, genes, gene2symbol) {
    assert(
        hasRownames(x),
        is.character(genes),
        is(gene2symbol, "Gene2Symbol"),
        identical(x = nrow(x), y = nrow(gene2symbol))
    )
    if (is.null(rownames(gene2symbol))) {
        rownames(gene2symbol) <- rownames(x)
    }
    # Map genes to x rownames, using gene2symbol.
    rownames <- mapGenesToRownames(object = gene2symbol, genes = genes)
    assert(isSubset(rownames, rownames(x)))
    TRUE
}



#' @rdname matchesGene2Symbol
#' @export
matchesGene2Symbol <- makeTestFunction(.matchesGene2Symbol)
