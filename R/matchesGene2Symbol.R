#' Check that user-defined gene input matches expected values
#'
#' @note Updated 2020-10-05.
#' @export
#'
#' @inherit goalie::check return
#' @inheritParams AcidRoxygen::params
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
#'         "geneId" = c("ENSG00000000003", "ENSG00000000005"),
#'         "geneName" = c("TSPAN6", "TNMD"),
#'         row.names = rownames(x)
#'     )
#' )
#' print(g2s)
#'
#' geneIds <- g2s[["geneId"]]
#' print(geneIds)
#'
#' geneNames <- g2s[["geneName"]]
#' print(geneNames)
#'
#' matchesGene2Symbol(x = x, genes = geneIds, gene2symbol = g2s)
#' matchesGene2Symbol(x = x, genes = geneNames, gene2symbol = g2s)
matchesGene2Symbol <- function(
    x,
    genes,
    gene2symbol,
    .xname = getNameInParent(x)
) {
    ok <- hasRownames(x)
    if (!isTRUE(ok)) {
        return(false("'%s' doesn't have row names.", .xname))
    }
    ok <- isCharacter(genes)
    if (!isTRUE(ok)) return(ok)
    ok <- is(gene2symbol, "Gene2Symbol")
    if (!isTRUE(ok)) {
        return(false("'gene2symbol' must be Gene2Symbol S4 class."))
    }
    ok <- identical(nrow(x), nrow(gene2symbol))
    if (!isTRUE(ok)) {
        return(false("Row mismatch between 'x' and 'gene2symbol'."))
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
        return(false("Failed to map genes to rownames in '%s'.", .xname))
    }
    TRUE
}
