# assertFormalGene2Symbol ======================================================
#' Check Gene-to-Symbol Mappings
#'
#' @inherit params
#' @export
#'
#' @param x Object class supporting `rownames`. All rownames in this object
#'   must intersect with the rownames defined in the `gene2symbol` argument.
#'
#' @examples
#' DataFrame <- S4Vectors::DataFrame
#' Gene2Symbol <- basejump::Gene2Symbol
#'
#' x <- DataFrame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(x)
#'
#' gene2symbol <- Gene2Symbol(
#'     x = DataFrame(
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
#' assertFormalGene2Symbol(
#'     x = x,
#'     genes = geneIDs,
#'     gene2symbol = gene2symbol
#' )
#' assertFormalGene2Symbol(
#'     x = x,
#'     genes = geneNames,
#'     gene2symbol = gene2symbol
#' )
assertFormalGene2Symbol <- function(
    x,
    genes,
    gene2symbol
) {
    requireNamespace("basejump", quietly = TRUE)
    assertHasRownames(x)
    assert_is_character(genes)
    assert_is_non_empty(genes)
    assert_is_all_of(gene2symbol, "Gene2Symbol")
    assert_are_identical(
        x = nrow(x),
        y = nrow(gene2symbol)
    )
    if (is.null(rownames(gene2symbol))) {
        rownames(gene2symbol) <- rownames(x)
    }
    # Map genes to x rownames, using gene2symbol.
    rownames <- basejump::mapGenesToRownames(
        x = gene2symbol,
        genes = genes
    )
    assert_is_subset(rownames, rownames(x))
    invisible()
}



# assertFormalInterestingGroups ================================================
#' Check Interesting Groups
#'
#' Prevent unwanted downstream behavior when a missing interesting group
#' is requested by the user.
#'
#' @inherit params
#' @export
#'
#' @param x S4 class x.
#' @param interestingGroups `character`. Interesting groups.
#'
#' @examples
#' data(rse, package = "basejump")
#' assertFormalInterestingGroups(rse, "treatment")
#' assertFormalInterestingGroups(rse, NULL)
assertFormalInterestingGroups <- function(x, interestingGroups) {
    requireNamespace("basejump", quietly = TRUE)
    assert_that(isS4(x))
    data <- basejump::sampleData(x)

    # Check `interestingGroups` argument.
    if (is.null(interestingGroups)) {
        # Early return clean on `NULL` value (e.g. DESeqDataSet).
        return(invisible())
    } else {
        # Otherwise, require that `interestingGroups` is a character.
        assert_is_character(interestingGroups)
    }

    # Check intersection with sample data.
    assert_is_subset(interestingGroups, colnames(data))

    # Check that interesting groups columns are factors.
    invisible(lapply(
        X = data[, interestingGroups, drop = FALSE],
        FUN = assert_is_factor
    ))
}
