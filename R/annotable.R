#' Ensembl Annotations
#'
#' @family Gene Functions
#' @author Michael Steinbaugh
#'
#' @inherit makeGRangesFromEnsembl
#'
#' @return `data.frame`
#' @export
#'
#' @examples
#' # Genes ====
#' x <- annotable("Homo sapiens")
#' glimpse(x)
#'
#' # Transcripts ====
#' x < - annotable("Homo sapiens", format = "transcripts")
#' glimpse(x)
annotable <- function(
    organism,
    format = c("genes", "transcripts"),
    genomeBuild = NULL,
    release = NULL
) {
    format <- match.arg(format)
    gr <- makeGRangesFromEnsembl(
        organism = organism,
        format = format,
        genomeBuild = genomeBuild,
        release = release
    )
    as.data.frame(gr)
}
