#' Gene-to-Symbol Mappings from Ensembl
#'
#' @inheritParams makeGRangesFromEnsembl
#'
#' @return `data.frame`.
#' @export
makeGene2symbolFromEnsembl <- function(
    organism,
    genomeBuild = NULL,
    release = NULL
) {
    gr <- makeGRangesFromEnsembl(
        organism = organism,
        format = "genes",
        genomeBuild = genomeBuild,
        release = NULL
    )
    mcols(gr) %>%
        .[, c("geneID", "geneName")] %>%
        as.data.frame() %>%
        set_rownames(.[[1L]])
}
