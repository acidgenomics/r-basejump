#' @inherit HGNC2Ensembl-class
#'
#' @family Identifier Mapping Functions
#' @export
#'
#' @inheritParams general
#'
#' @return `HGNC2Ensembl`.
#'
#' @examples
#' x <- HGNC2Ensembl(.test = TRUE)
#' print(x)
HGNC2Ensembl <- function(.test = FALSE) {
    stopifnot(has_internet())
    assert_is_a_bool(.test)

    if (isTRUE(.test)) {
        file <- file.path(basejumpCacheURL, "hgnc.txt.gz")
    } else {
        file <- .hgncURL
    }

    message("Obtaining HGNC to Ensembl gene ID mappings.")
    data <- read_tsv(
        file = file,
        # Suppress the column messages.
        col_types = cols(),
        progress = FALSE
    )
    data <- as.data.frame(data)
    data <- camel(data)
    data <- data[, c("hgncID", "ensemblGeneID")]
    colnames(data)[[2L]] <- "geneID"
    data <- data[!is.na(data[["geneID"]]), ]
    data[["hgncID"]] <- as.integer(gsub("^HGNC\\:", "", data[["hgncID"]]))
    data <- data[order(data[["hgncID"]]), ]
    data <- as(data, "DataFrame")

    out <- new(Class = "HGNC2Ensembl", data)
    metadata(out) <- .prototypeMetadata
    out
}



.hgncURL <- paste(
    "ftp://ftp.ebi.ac.uk",
    "pub",
    "databases",
    "genenames",
    "new",
    "tsv",
    "hgnc_complete_set.txt",
    sep = "/"
)
