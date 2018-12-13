#' Check Gene-to-Symbol Value Matching
#'
#' @name matchesGene2Symbol
#' @inherit params
#'
#' @param x Object class supporting `rownames`.
#'   All rownames in this object must intersect with the rownames defined in the
#'   `gene2symbol` argument.
#'
#' @examples
#' x <- S4Vectors::DataFrame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(x)
#'
#' g2s <- Gene2Symbol(
#'     object = S4Vectors::DataFrame(
#'         geneID = c("ENSG00000000003", "ENSG00000000005"),
#'         geneName = c("TSPAN6", "TNMD"),
#'         row.names = rownames(x)
#'     )
#' )
#' print(g2s)
#'
#' geneIDs <- g2s[["geneID"]]
#' print(geneIDs)
#'
#' geneNames <- g2s[["geneName"]]
#' print(geneNames)
#'
#' matchesGene2Symbol(x = x, genes = geneIDs, gene2symbol = g2s)
#' matchesGene2Symbol(x = x, genes = geneNames, gene2symbol = g2s)
NULL



.matchesGene2Symbol <- function(x, genes, gene2symbol) {
    ok <- hasRownames(x)
    if (!isTRUE(ok)) {
        return("x doesn't have rownmaes.")
    }

    ok <- isCharacter(genes)
    if (!isTRUE(ok)) {
        return("genes must be non-empty character.")
    }

    ok <- is(gene2symbol, "Gene2Symbol")
    if (!isTRUE(ok)) {
        return("gene2symbol must be Gene2Symbol S4 class.")
    }

    ok <- identical(nrow(x), nrow(gene2symbol))
    if (!isTRUE(ok)) {
        return("Row mismatch between x and gene2symbol detected.")
    }

    # Consider tightening up this step.
    if (is.null(rownames(gene2symbol))) {
        rownames(gene2symbol) <- rownames(x)
    }

    # Map genes to x rownames, using gene2symbol.
    ok <- isSubset(
        x = mapGenesToRownames(object = gene2symbol, genes = genes),
        y = rownames(x)
    )
    if (!isTRUE(ok)) {
        return("Failed to map genes to rownames in x.")
    }

    TRUE
}



#' @rdname matchesGene2Symbol
#' @export
matchesGene2Symbol <- makeTestFunction(.matchesGene2Symbol)
