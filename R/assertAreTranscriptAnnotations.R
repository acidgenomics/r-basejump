#' Assert Are Ensembl Transcript Annotations
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl transcript annotations.
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'     "txID" = "ENST00000000233",
#'     "geneID" = "ENSG00000004059"
#' )
#' assertAreTranscriptAnnotations(x)
assertAreTranscriptAnnotations <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    x <- as.data.frame(x)
    assert_is_subset(
        x = c("txID", "geneID"),
        y = colnames(x),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}
