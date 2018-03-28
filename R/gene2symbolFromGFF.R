#' Define Gene to Symbol Mappings from GFF/GTF File
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
#' gene2symbolFromGFF("http://basejump.seq.cloud/mmusculus.gtf") %>% glimpse()
gene2symbolFromGFF <- function(file) {
    gff <- readGFF(file)
    data <- .gffKeyValuePairs(gff, unique = TRUE)

    # Standardize `geneName` column (Ensembl format).
    # This fix is necessary for FlyBase GFF files.
    if ("geneSymbol" %in% colnames(data)) {
        data[["geneName"]] <- data[["geneSymbol"]]
    }

    data <- data[, c("geneID", "geneName")] %>%
        mutate_if(is.factor, as.character) %>%
        .[!is.na(.[["geneID"]]), , drop = FALSE] %>%
        .[!is.na(.[["geneName"]]), , drop = FALSE] %>%
        unique() %>%
        .[order(.[["geneID"]]), , drop = FALSE] %>%
        set_rownames(.[["geneID"]])
    assert_has_no_duplicates(data[["geneID"]])

    inform(paste(
        "gene2symbol mappings:",
        length(unique(data[["geneID"]])), "genes"
    ))

    data
}



# Aliases ======================================================================
#' @rdname gene2symbolFromGFF
#' @export
gene2symbolFromGFF -> gene2symbolFromGTF
