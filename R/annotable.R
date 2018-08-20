#' Ensembl Annotations
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inherit makeGRangesFromEnsembl
#'
#' @param ... Passthrough to [makeGRangesFromEnsembl()].
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' # Genes ====
#' x <- annotable("Homo sapiens")
#' glimpse(x)
#'
#' # Transcripts ====
#' x <- annotable("Homo sapiens", format = "transcripts")
#' glimpse(x)
annotable <- function(...) {
    as.data.frame(makeGRangesFromEnsembl(...))
}
