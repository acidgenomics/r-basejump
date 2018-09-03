#' MGI to Ensembl Gene ID Mappings
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @return `DataFrame`.
#'
#' @examples
#' x <- mgi2gene()
#' print(x)
mgi2gene <- function() {
    stopifnot(has_internet())
    message("Obtaining MGI to Ensembl gene ID mappings")
    data <- read_tsv(
        file = paste(
            "http://www.informatics.jax.org",
            "downloads",
            "reports",
            "MGI_Gene_Model_Coord.rpt",
            sep = "/"
        ),
        # Note the use of `null` here
        na = c("", "NA", "null"),
        col_names = FALSE,
        # Suppress the column messages.
        col_types = cols(),
        skip = 1L
    )
    data <- as(data[, c(1L, 11L)], "DataFrame")
    colnames(data) <- c("mgiID", "geneID")
    data[["mgiID"]] <- as.integer(gsub("^MGI\\:", "", data[["mgiID"]]))
    data <- data[order(data[["mgiID"]]), , drop = FALSE]
    rownames(data) <- data[["mgiID"]]
    data
}
