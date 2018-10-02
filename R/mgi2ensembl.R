#' MGI to Ensembl Gene ID Mappings
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @seealso `mgi2ensembl-class`.
#'
#' @return `mgi2ensembl`.
#'
#' @examples
#' x <- mgi2ensembl()
#' print(x)
mgi2ensembl <- function(.test = FALSE) {
    stopifnot(has_internet())
    assert_is_a_bool(.test)

    if (isTRUE(.test)) {
        file <- "mgi.rpt.gz"
        assert_all_are_existing_files(file)
    } else {
        file <- paste(
            "http://www.informatics.jax.org",
            "downloads",
            "reports",
            "MGI_Gene_Model_Coord.rpt",
            sep = "/"
        )
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
    rownames(data) <- data[["mgiID"]]

    new("mgi2ensembl", data)
}
