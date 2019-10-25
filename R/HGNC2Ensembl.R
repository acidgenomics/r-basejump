#' @inherit HGNC2Ensembl-class title description return
#' @note Updated 2019-08-08.
#' @export
#' @inheritParams acidroxygen::params
#' @examples
#' options(acid.test = TRUE)
#' x <- HGNC2Ensembl()
#' print(x)
HGNC2Ensembl <-  # nolint
    function() {
        assert(hasInternet())
        if (isTRUE(getOption("acid.test"))) {
            file <- pasteURL(
                basejumpTestsURL, "hgnc.txt.gz",
                protocol = "none"
            )
        } else {
            ## This is unreliable on Travis, so cover locally instead.
            ## nocov start
            file <- pasteURL(
                "ftp.ebi.ac.uk",
                "pub",
                "databases",
                "genenames",
                "new",
                "tsv",
                "hgnc_complete_set.txt",
                protocol = "ftp"
            )
            ## nocov end
        }
        message("Importing HGNC to Ensembl gene ID mappings.")
        data <- withCallingHandlers(
            expr = import(file),
            message = function(m) {
                if (isTRUE(grepl(pattern = "syntactic", x = m))) {
                    invokeRestart("muffleMessage")
                } else {
                    m
                }
            }
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
