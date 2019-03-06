#' @rdname HGNC2Ensembl-class
#' @export
#' @inheritParams params
#' @examples
#' options(basejump.test = TRUE)
#' x <- HGNC2Ensembl()
#' print(x)
HGNC2Ensembl <-  # nolint
    function() {
        assert(hasInternet())

        if (isTRUE(getOption("basejump.test"))) {
            file <- pasteURL(
                basejumpCacheURL, "hgnc.txt.gz",
                protocol = "none"
            )
        } else {
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
        }

        message("Obtaining HGNC to Ensembl gene ID mappings.")
        # nolint start
        # `readr::read_tsv()` has parsing issues with this file.
        #> data <- read_tsv(
        #>     file = file,
        #>     col_types = cols(),
        #>     progress = FALSE
        #> )
        #>> Warning: 5075 parsing failures.
        #>> row                      col           expected    actual                                                                        file
        #>> 2200 kznf_gene_catalog        1/0/T/F/TRUE/FALSE 404       'ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt'
        #>> 2210 kznf_gene_catalog        1/0/T/F/TRUE/FALSE 341       'ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt'
        #>> 2211 kznf_gene_catalog        1/0/T/F/TRUE/FALSE 90        'ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt'
        #>> 2276 intermediate_filament_db 1/0/T/F/TRUE/FALSE HGNC:1040 'ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt'
        #>> 2277 intermediate_filament_db 1/0/T/F/TRUE/FALSE HGNC:1041 'ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt'
        #
        # `data.table::fread()` appears to load clean, with no warnings.
        # nolint end
        data <- import(file)
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
