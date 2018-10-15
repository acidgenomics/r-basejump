#' @inherit MGI2Ensembl-class
#'
#' @family Identifier Mapping Functions
#' @export
#'
#' @inheritParams general
#'
#' @return `MGI2Ensembl`.
#'
#' @examples
#' x <- MGI2Ensembl(.test = TRUE)
#' print(x)
MGI2Ensembl <- function(.test = FALSE) {
    stopifnot(has_internet())
    assert_is_a_bool(.test)

    if (isTRUE(.test)) {
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

    out <- new(Class = "MGI2Ensembl", data)
    metadata(out) <- .prototypeMetadata
    out
}



.mgiURL <- paste(
    "http://www.informatics.jax.org",
    "downloads",
    "reports",
    "MGI_Gene_Model_Coord.rpt",
    sep = "/"
)
