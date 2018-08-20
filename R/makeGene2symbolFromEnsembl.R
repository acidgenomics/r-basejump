#' Gene-to-Symbol Mappings from Ensembl
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
#' x <- makeGene2symbolFromEnsembl("Homo sapiens")
#' glimpse(x)
makeGene2symbolFromEnsembl <- function(...) {
    gr <- makeGRangesFromEnsembl(..., format = "genes")
    mcols(gr) %>%
        .[, c("geneID", "geneName")] %>%
        as.data.frame() %>%
        mutate_all(as.character) %>%
        set_rownames(.[[1L]])
}
