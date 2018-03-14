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
#' x <- ensembl("Homo sapiens", format = "genes", genomeBuild = "GRCh37")
#' assertAreGeneAnnotations(x)
assertAreGeneAnnotations <- function(x, severity = "stop") {
    x <- as.data.frame(x)
    assert_is_subset(
        x = c("geneID", "geneName"),
        y = colnames(x),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}
