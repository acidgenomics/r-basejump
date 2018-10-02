#' HGNC to Ensembl Gene ID Mappings.
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `hgnc2ensembl`.
#'
#' @examples
#' x <- hgnc2ensembl(.test = TRUE)
#' print(x)
hgnc2ensembl <- function(.test = FALSE) {
    stopifnot(has_internet())
    assert_is_a_bool(.test)

    if (isTRUE(.test)) {
        file <- file.path(basejumpCacheURL, "hgnc.txt.gz")
    } else {
        file <- paste(
            "ftp://ftp.ebi.ac.uk",
            "pub",
            "databases",
            "genenames",
            "new",
            "tsv",
            "hgnc_complete_set.txt",
            sep = "/"
        )
    }

    message("Obtaining HGNC to Ensembl gene ID mappings...")
    data <- read_tsv(
        file = file,
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
