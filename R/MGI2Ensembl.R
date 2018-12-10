#' @inherit MGI2Ensembl-class
#' @export
#' @inheritParams params
#' @examples
#' options(basejump.test = TRUE)
#' x <- MGI2Ensembl()
#' print(x)
MGI2Ensembl <- function() {  # nolint
    assert(hasInternet())

    if (isTRUE(getOption("basejump.test"))) {
        file <- file.path(basejumpCacheURL, "mgi.rpt.gz")
    } else {
        file <- paste(
            "http://www.informatics.jax.org",
            "downloads",
            "reports",
            "MGI_Gene_Model_Coord.rpt",
            sep = "/"
        )
    }

    message("Obtaining MGI to Ensembl gene ID mappings.")
    data <- read_tsv(
        file = file,
        # Using our global NA strings.
        na = naStrings,
        col_names = FALSE,
        # Suppress the column messages.
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
