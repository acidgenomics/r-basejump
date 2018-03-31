#' Transcript-to-Gene Mappings from Ensembl
#'
#' @family Gene Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams makeGRangesFromEnsembl
#'
#' @return `data.frame`
#' @export
makeTx2geneFromEnsembl <- function(
    organism,
    genomeBuild = NULL,
    release = NULL
) {
    gr <- makeGRangesFromEnsembl(
        organism = organism,
        format = "transcripts",
        genomeBuild = genomeBuild,
        release = NULL
    )
    mcols(gr) %>%
        .[, c("txID", "geneID")] %>%
        as.data.frame() %>%
        set_rownames(.[[1L]])
}
