#' `MGI2Ensembl` Generator
#'
#' @family S4 Generators
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `MGI2Ensembl`.
#'
#' @examples
#' x <- mgi2ensembl(.test = TRUE)
#' print(x)
mgi2ensembl <- function(.test = FALSE) {
    stopifnot(has_internet())
    assert_is_a_bool(.test)

    if (isTRUE(.test)) {
        file <- file.path(basejumpCacheURL, "mgi.rpt.gz")
    } else {
        file <- .mgiURL
    }

    message("Obtaining MGI to Ensembl gene ID mappings...")
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

    new(Class = "MGI2Ensembl", data)
}



#' @rdname mgi2ensembl
#' @usage NULL
#' @export
MGI2Ensembl <- mgi2ensembl



.mgiURL <- paste(
    "http://www.informatics.jax.org",
    "downloads",
    "reports",
    "MGI_Gene_Model_Coord.rpt",
    sep = "/"
)
