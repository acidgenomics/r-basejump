#' Gene-to-Symbol Mappings from Ensembl
#'
#' @family Gene Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams makeGRangesFromEnsembl
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' x <- makeGene2symbolFromEnsembl("Homo sapiens")
#' glimpse(x)
makeGene2symbolFromEnsembl <- function(
    organism,
    genomeBuild = NULL,
    release = NULL
) {
    gr <- makeGRangesFromEnsembl(
        organism = organism,
        format = "genes",
        genomeBuild = genomeBuild,
        release = release
    )
    mcols(gr) %>%
        .[, c("geneID", "geneName")] %>%
        as.data.frame() %>%
        mutate_all(as.character) %>%
        set_rownames(.[[1L]])
}
