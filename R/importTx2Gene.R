#' Import transcript-to-gene annotations
#'
#' Generates a `Tx2Gene` object containing `transcriptID` and `geneID` columns.
#'
#' @note File should not contain column header names.
#' @note The functon doesn't attempt to strip transcript versions. Use
#'   [stripTranscriptVersions()] in a separate call, if necessary.
#' @note Updated 2019-08-13.
#' @export
#'
#' @inheritParams acidroxygen::params
#'
#' @return `Tx2Gene`.
#'
#' @examples
#' file <- file.path(basejumpTestsURL, "tx2gene.csv")
#' x <- importTx2Gene(
#'     file = file,
#'     organism = "Mus musculus",
#'     genomeBuild = "GRCm38",
#'     ensemblRelease = 90L
#' )
#' print(x)
importTx2Gene <- function(
    file,
    organism = NULL,
    genomeBuild = NULL,
    ensemblRelease = NULL
) {
    data <- import(
        file = file,
        rownames = FALSE,
        colnames = FALSE
    )
    colnames(data) <- c("transcriptID", "geneID")
    data <- as(data, "DataFrame")
    metadata(data) <- list(
        organism = as.character(organism),
        genomeBuild = as.character(genomeBuild),
        ensemblRelease = as.integer(ensemblRelease)
    )
    Tx2Gene(data)
}
