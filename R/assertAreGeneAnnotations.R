#' Assert Are Ensembl Gene Annotations
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param x [data.frame] containing Ensembl gene annotations returned
#'   by the [genes()] function.
#'
#' @export
#'
#' @examples
#' # Success
#' genes <- genes("Homo sapiens")
#' assertAreGeneAnnotations(genes)
#'
#' # Failure
#' transcripts <- transcripts("Homo sapiens")
#' tryCatch(
#'     assertAreGeneAnnotations(transcripts),
#'     error = function(e) e)
assertAreGeneAnnotations <- function(x, severity = "stop") {
    x <- as.data.frame(x)
    assert_is_subset(
        x = geneAnnotationCols,
        y = colnames(x),
        severity = severity)
    assert_has_rows(x, severity = severity)
}
