#' Gene-to-Symbol Mappings from GFF File
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @family Gene Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- makeGene2symbolFromGFF("http://basejump.seq.cloud/mmusculus.gtf")
#' glimpse(x)
#' }
makeGene2symbolFromGFF <- function(file) {
    message("Making gene2symbol from GFF")

    data <- readGFF(file) %>%
        as.data.frame() %>%
        select(starts_with("gene_")) %>%
        unique() %>%
        camel()

    # Standardize columns into Ensembl format
    if ("geneSymbol" %in% colnames(data)) {
        data[["geneName"]] <- data[["geneSymbol"]]
    }

    data[, c("geneID", "geneName")] %>%
        # Drop rows containing an NA value
        .[complete.cases(.), , drop = FALSE] %>%
        .[order(.[["geneID"]]), , drop = FALSE] %>%
        set_rownames(.[["geneID"]])
}



# Aliases ======================================================================
#' @rdname makeGene2symbolFromGFF
#' @usage NULL
#' @export
makeGene2symbolFromGFF -> makeGene2symbolFromGTF
