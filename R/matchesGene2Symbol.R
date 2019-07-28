#' Check that user-defined gene input matches expected values
#'
#' @note Updated 2019-07-28.
#' @export
#'
#' @inheritParams params
#' @inheritParams goalie::params
#' @param x Object class supporting [`rownames()`][base::rownames].
#'   All rownames in this object must intersect with the rownames defined in the
#'   `gene2symbol` argument.
#'
#' @return `logical(1)`.
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
matchesGene2Symbol <- function(
    x,
    genes,
    gene2symbol,
    .xname = getNameInParent(x)
) {
    ok <- hasRownames(x)
    if (!isTRUE(ok)) {
        return(false("%s doesn't have row names.", .xname))
    }

    ok <- isCharacter(genes)
    if (!isTRUE(ok)) {
        return(false(
            "genes is not non-empty character: %s",
            toString(genes)
        ))
    }

    ok <- is(gene2symbol, "Gene2Symbol")
    if (!isTRUE(ok)) {
        return(false("gene2symbol must be Gene2Symbol S4 class."))
    }

    ok <- identical(nrow(x), nrow(gene2symbol))
    if (!isTRUE(ok)) {
        return(false("Row mismatch between x and gene2symbol."))
    }

    ## Consider tightening up this step.
    if (is.null(rownames(gene2symbol))) {
        rownames(gene2symbol) <- rownames(x)
    }

    ## Map genes to x rownames, using gene2symbol.
    ok <- isSubset(
        x = mapGenesToRownames(object = gene2symbol, genes = genes),
        y = rownames(x)
    )
    if (!isTRUE(ok)) {
        return(false("Failed to map genes to rownames in %s.", .xname))
    }

    TRUE
}
