#' @inherit HGNC2Ensembl-class
#' @inheritParams params
#' @export
#' @examples
#' options(basejump.test = TRUE)
#' x <- HGNC2Ensembl()
#' print(x)
HGNC2Ensembl <-  # nolint
    function() {
        assert_that(has_internet())

        if (isTRUE(getOption("basejump.test"))) {
            file <- file.path(basejumpCacheURL, "hgnc.txt.gz")
        } else {
            file <- paste(
                "ftp://ftp.ebi.ac.uk",
                "pub",
                "databases",
                "genenames",
                "new",
                "tsv",
                "hgnc_complete_set.txt",
                sep = "/"
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
        data <- data[!is.na(data[["geneID"]]), ]
        data[["hgncID"]] <- as.integer(gsub("^HGNC\\:", "", data[["hgncID"]]))
        data <- data[order(data[["hgncID"]]), ]
        data <- as(data, "DataFrame")
        metadata(data) <- .prototypeMetadata

        new(Class = "HGNC2Ensembl", data)
    }
