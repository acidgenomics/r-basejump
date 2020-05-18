#' @inherit MGI2Ensembl-class title description return
#' @note Updated 2020-05-18.
#' @export
#' @examples
#' options(acid.test = TRUE)
#' x <- MGI2Ensembl()
#' print(x)
MGI2Ensembl <- function() {  # nolint
    assert(hasInternet())
    if (isTRUE(getOption("acid.test"))) {
        file <- pasteURL(basejumpTestsURL, "mgi.rpt.gz", protocol = "none")
    } else {
        file <- pasteURL(
            "www.informatics.jax.org",
            "downloads",
            "reports",
            "MGI_Gene_Model_Coord.rpt",
            protocol = "http"
        )
    }
    cli_alert("Importing MGI-to-Ensembl gene ID mappings.")
    data <- import(file = file, format = "tsv", colnames = TRUE)
    data <- as(data[, c(1L, 11L)], "DataFrame")
    colnames(data) <- c("mgiID", "geneID")
    data[["mgiID"]] <- as.integer(gsub("^MGI\\:", "", data[["mgiID"]]))
    data <- data[order(data[["mgiID"]]), , drop = FALSE]
    metadata(data) <- .prototypeMetadata
    new(Class = "MGI2Ensembl", data)
}
