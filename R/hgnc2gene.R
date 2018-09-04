#' HGNC to Ensembl Gene ID Mappings
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @return `DataFrame`.
#' @export
#'
#' @examples
#' x <- hgnc2gene()
#' print(x)
hgnc2gene <- function() {
    stopifnot(has_internet())
    message("Obtaining HGNC to Ensembl gene ID mappings")
    data <- read_tsv(
        file = paste(
            "ftp://ftp.ebi.ac.uk",
            "pub",
            "databases",
            "genenames",
            "new",
            "tsv",
            "hgnc_complete_set.txt",
            sep = "/"
        ),
        # Suppress the column messages.
        col_types = cols()
    )
    data <- as(data, "tbl_df")
    data <- camel(data)
    data <- data[, c("hgncID", "ensemblGeneID")]
    colnames(data)[[2L]] <- "geneID"
    data <- data[!is.na(data[["geneID"]]), ]
    data[["hgncID"]] <- as.integer(gsub("^HGNC\\:", "", data[["hgncID"]]))
    data <- data[order(data[["hgncID"]]), ]
    data <- as(data, "DataFrame")
    rownames(data) <- data[["hgncID"]]
    data
}
