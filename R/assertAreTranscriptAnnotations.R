#' Assert Are Ensembl Transcript Annotations
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl transcript annotations.
#'
#' @export
#'
#' @examples
#' x <- ensembl("Homo sapiens", format = "transcripts", genomeBuild = "GRCh37")
#' assertAreTranscriptAnnotations(x)
assertAreTranscriptAnnotations <- function(x, severity = "stop") {
    x <- as.data.frame(x)
    assert_is_subset(
        x = c("txID", "geneID"),
        y = colnames(x),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}
