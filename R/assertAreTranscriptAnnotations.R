#' Assert Are Ensembl Transcript Annotations
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param x [data.frame] containing Ensembl transcript annotations returned
#'   by the [transcripts()] function.
#'
#' @export
#'
#' @examples
#' # Success
#' transcripts <- transcripts("Homo sapiens")
#' assertAreTranscriptAnnotations(transcripts)
#'
#' # Failure
#' genes <- genes("Homo sapiens")
#' tryCatch(
#'     assertAreTranscriptAnnotations(genes),
#'     error = function(e) e
#' )
assertAreTranscriptAnnotations <- function(x, severity = "stop") {
    x <- as.data.frame(x)
    assert_is_subset(
        x = transcriptAnnotationCols,
        y = colnames(x),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}
