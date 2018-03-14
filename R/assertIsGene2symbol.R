#' Assert Is Gene to Symbol Mapping Data Frame
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl gene-to-symbol mappings. Must be
#'   structured as a two column `data.frame` with "geneID" and "geneName"
#'   columns.
#'
#' @export
#'
#' @examples
#' gene2symbol <- gene2symbol("Homo sapiens", genomeBuild = "GRCh37")
#' assertIsGene2symbol(gene2symbol)
assertIsGene2symbol <- function(x, severity = "stop") {
    assert_is_data.frame(x, severity = severity)
    assert_are_identical(
        x = colnames(x),
        y = c("geneID", "geneName"),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}
