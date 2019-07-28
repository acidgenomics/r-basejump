#' @rdname MGI2Ensembl-class
#' @note Updated 2019-07-28.
#' @export
#' @inheritParams params
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

    message("Obtaining MGI to Ensembl gene ID mappings.")
    data <- read_tsv(
        file = file,
        ## Using our global NA strings.
        na = naStrings,
        col_names = FALSE,
        ## Suppress the column messages.
        col_types = cols(),
        skip = 1L,
        progress = FALSE
    )
    data <- as(data[, c(1L, 11L)], "DataFrame")
    colnames(data) <- c("mgiID", "geneID")
    data[["mgiID"]] <- as.integer(gsub("^MGI\\:", "", data[["mgiID"]]))
    data <- data[order(data[["mgiID"]]), , drop = FALSE]
    metadata(data) <- .prototypeMetadata

    new(Class = "MGI2Ensembl", data)
}
