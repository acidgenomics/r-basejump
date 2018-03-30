#' Gene-to-Symbol Mappings from GFF File
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @family GFF Functions
#'
#' @inheritParams general
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' makeGene2symbolFromGFF("http://basejump.seq.cloud/mmusculus.gtf") %>%
#'     glimpse()
makeGene2symbolFromGFF <- function(file) {
    inform("Making gene2symbol from GFF")
    data <- parseGFFAttributes(file, select = "gene_") %>%
        as.data.frame() %>%
        camel()

    # Standardize columns into Ensembl format
    if ("geneSymbol" %in% colnames(data)) {
        data[["geneName"]] <- data[["geneSymbol"]]
    }

    data[, c("geneID", "geneName")] %>%
        # Drop rows containing an NA value
        .[complete.cases(.), , drop = FALSE] %>%
        mutate_if(is.factor, as.character) %>%
        unique() %>%
        .[order(.[["geneID"]]), , drop = FALSE] %>%
        set_rownames(.[["geneID"]])
}



# Aliases ======================================================================
#' @rdname makeGene2symbolFromGFF
#' @export
makeGene2symbolFromGFF -> makeGene2symbolFromGTF
