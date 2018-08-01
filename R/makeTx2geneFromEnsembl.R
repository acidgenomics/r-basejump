#' Transcript-to-Gene Mappings from Ensembl
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams makeGRangesFromEnsembl
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' x <- makeTx2geneFromEnsembl("Homo sapiens")
#' glimpse(x)
makeTx2geneFromEnsembl <- function(
    organism,
    genomeBuild = NULL,
    release = NULL
) {
    gr <- makeGRangesFromEnsembl(
        organism = organism,
        format = "transcripts",
        genomeBuild = genomeBuild,
        release = release
    )
    mcols(gr) %>%
        as.data.frame() %>%
        select(!!!syms(c("transcriptID", "geneID"))) %>%
        mutate_all(as.character) %>%
        set_rownames(.[[1L]])
}
