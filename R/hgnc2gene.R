#' HGNC to Ensembl Gene ID Mappings
#'
#' @author Michael Steinbaugh
#'
#' @return `data.frame`
#' @export
#'
#' @examples
#' x <- hgnc2gene()
#' glimpse(x)
hgnc2gene <- function() {
    raw <- read_tsv(
        file = paste(
            "ftp://ftp.ebi.ac.uk",
            "pub",
            "databases",
            "genenames",
            "new",
            "tsv",
            "hgnc_complete_set.txt",
            sep = "/"
        )
    )
    data <- as.data.frame(raw)
    data <- camel(data)
    data <- data[, c("hgncID", "ensemblGeneID")]
    colnames(data)[[2L]] <- "geneID"
    data <- data[!is.na(data[["geneID"]]), ]
    data[["hgncID"]] <- as.integer(gsub("^HGNC\\:", "", data[["hgncID"]]))
    data <- data[order(data[["hgncID"]]), ]
    rownames(data) <- data[["hgncID"]]
    data
}
