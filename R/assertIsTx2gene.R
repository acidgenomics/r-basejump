#' Check Transcript to Gene Mapping Data
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl transcript to gene identifier
#'   mappings. Must be structured as a two column `data.frame` with "txID" and
#'   "geneID" columns.
#'
#' @export
#'
#' @examples
#' tx2gene <- tx2gene("Homo sapiens", genomeBuild = "GRCh37")
#' assertIsTx2gene(tx2gene)
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
