#' Transcript-to-Gene Mappings from Ensembl
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams makeGRangesFromEnsembl
#' @param ... Passthrough to [makeGRangesFromEnsembl()].
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' x <- makeTx2geneFromEnsembl("Homo sapiens")
#' glimpse(x)
makeTx2geneFromEnsembl <- function(...) {
    gr <- makeGRangesFromEnsembl(..., format = "transcripts")
    mcols(gr) %>%
        as.data.frame() %>%
        select(!!!syms(c("transcriptID", "geneID"))) %>%
        mutate_all(as.character) %>%
        set_rownames(.[[1L]])
}
