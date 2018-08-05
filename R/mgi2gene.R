#' MGI to Ensembl Gene ID Mappings
#'
#' @family Annotation Functions
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' x <- mgi2gene()
#' glimpse(x)
mgi2gene <- function() {
    stopifnot(has_internet())
    message("Obtaining MGI to Ensembl gene ID mappings")
    raw <- read_tsv(
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
        skip = 1L
    )
    data <- as.data.frame(raw[, c(1L, 11L)])
    colnames(data) <- c("mgiID", "geneID")
    data[["mgiID"]] <- as.integer(gsub("^MGI\\:", "", data[["mgiID"]]))
    data <- data[order(data[["mgiID"]]), ]
    rownames(data) <- data[["mgiID"]]
    data
}
