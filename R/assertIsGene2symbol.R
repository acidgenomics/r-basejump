#' Assert Is Gene to Symbol Mapping Data Frame
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl gene-to-symbol mappings. Must be
#'   structured as a two column `data.frame` with "geneID" and "geneName"
#'   columns.
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'     "geneID" = "ENSG00000000003",
#'     "geneName" = "TSPAN6"
#' )
#' assertIsGene2symbol(x)
assertIsGene2symbol <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_data.frame(x, severity = severity)
    assert_are_identical(
        x = colnames(x),
        y = c("geneID", "geneName"),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}
