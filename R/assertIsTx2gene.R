#' Check Transcript to Gene Mapping Data
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl transcript to gene identifier
#'   mappings. Must be structured as a two column `data.frame` with "txID" and
#'   "geneID" columns.
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'     "txID" = "ENST00000000233",
#'     "geneID" = "ENSG00000004059"
#' )
#' assertIsTx2gene(x)
assertIsTx2gene <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_data.frame(x, severity = severity)
    assert_are_identical(
        x = colnames(x),
        y = c("txID", "geneID"),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}
