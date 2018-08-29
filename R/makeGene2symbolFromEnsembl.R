#' Gene-to-Symbol Mappings from Ensembl
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams makeGRangesFromEnsembl
#' @inheritParams gene2symbol
#' @param ... Passthrough to [makeGRangesFromEnsembl()].
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' x <- makeGene2symbolFromEnsembl("Homo sapiens")
#' glimpse(x)
makeGene2symbolFromEnsembl <- function(
    ...,
    unique = TRUE
) {
    gr <- makeGRangesFromEnsembl(..., format = "genes")
    data <- mcols(gr) %>%
        .[, c("geneID", "geneName")] %>%
        as.data.frame() %>%
        mutate_all(as.character) %>%
        arrange(!!sym("geneID")) %>%
        set_rownames(.[["geneID"]])

    # Ensure gene names (symbols) are unique, if desired.
    # This is recommended by default.
    if (isTRUE(unique)) {
        data <- .makeGeneNamesUnique(data)
    }

    assertIsGene2symbol(data)
    data
}
