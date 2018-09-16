#' HGNC to Ensembl Gene ID Mappings.
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @return `hgnc2ensembl`.
#'
#' @examples
#' x <- hgnc2ensembl()
#' print(x)
hgnc2ensembl <- function() {
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
        col_types = cols(),
        progress = FALSE
    )
    data <- as(data, "tbl_df")
    data <- camel(data)
    data <- data[, c("hgncID", "ensemblGeneID")]
    colnames(data)[[2L]] <- "geneID"
    data <- data[!is.na(data[["geneID"]]), ]
    data[["hgncID"]] <- as.integer(gsub("^HGNC\\:", "", data[["hgncID"]]))
    data <- data[order(data[["hgncID"]]), ]
    data <- as(data, "DataFrame")
    rownames(data) <- as.character(data[["hgncID"]])
    new("hgnc2ensembl", data)
}
