#' @inherit MGI2Ensembl-class
#'
#' @export
#'
#' @inheritParams general
#'
#' @return `MGI2Ensembl`.
#'
#' @examples
#' options(basejump.test = TRUE)
#' x <- MGI2Ensembl()
#' print(x)
MGI2Ensembl <- function() {
    stopifnot(has_internet())

    if (isTRUE(getOption("basejump.test"))) {
        file <- file.path(basejumpCacheURL, "mgi.rpt.gz")
    } else {
        file <- .mgiURL
    }

    message("Obtaining MGI to Ensembl gene ID mappings.")
    data <- read_tsv(
        file = file,
        # Using our global NA strings.
        na = na,
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



.mgiURL <- paste(
    "http://www.informatics.jax.org",
    "downloads",
    "reports",
    "MGI_Gene_Model_Coord.rpt",
    sep = "/"
)
