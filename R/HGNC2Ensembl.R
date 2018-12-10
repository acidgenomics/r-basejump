#' @inherit HGNC2Ensembl-class
#' @inheritParams params
#' @export
#' @examples
#' options(basejump.test = TRUE)
#' x <- HGNC2Ensembl()
#' print(x)
HGNC2Ensembl <-  # nolint
    function() {
        assert(hasInternet())

        if (isTRUE(getOption("basejump.test"))) {
            file <- url(
                basejumpCacheURL,
                "hgnc.txt.gz",
                protocol = "none"
            )
        } else {
            file <- url(
                "ftp.ebi.ac.uk",
                "pub",
                "databases",
                "genenames",
                "new",
                "tsv",
                "hgnc_complete_set.txt",
                protocol = "ftp"
            )
        }

        message("Obtaining HGNC to Ensembl gene ID mappings.")
        data <- read_tsv(
            file = file,
            # Suppress the column messages.
            col_types = cols(),
            progress = FALSE
        )
        data <- camel(data)
        data <- data[, c("hgncID", "ensemblGeneID")]
        colnames(data)[[2L]] <- "geneID"
        data <- data[!is.na(data[["geneID"]]), , drop = FALSE]
        data[["hgncID"]] <- as.integer(gsub("^HGNC\\:", "", data[["hgncID"]]))
        data <- data[order(data[["hgncID"]]), , drop = FALSE]
        data <- as(data, "DataFrame")
        metadata(data) <- .prototypeMetadata

        new(Class = "HGNC2Ensembl", data)
    }
