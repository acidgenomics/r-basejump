#' Assert Are Ensembl Gene Annotations
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl gene annotations.
#'
#' @export
#'
#' @examples
#' # Success ====
#' data <- ensembl("Homo sapiens", format = "genes")
#' assertAreGeneAnnotations(data)
#'
#' # Failure ====
#' data <- ensembl("Homo sapiens", format = "transcripts")
#' tryCatch(
#'     assertAreGeneAnnotations(data),
#'     error = function(e) e
#' )
assertAreGeneAnnotations <- function(x, severity = "stop") {
    x <- as.data.frame(x)
    assert_is_subset(
        x = c("geneID", "geneName"),
        y = colnames(x),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}
