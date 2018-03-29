# Switch to GenomicFeatures


#' Define Transcript to Gene Mappings from GFF/GTF File
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
#' tx2geneFromGFF("http://basejump.seq.cloud/mmusculus.gtf") %>% glimpse()
tx2geneFromGFF <- function(file) {
    data <- parseGFFAttributes(
        file = file,
        select = c("gene_id", "transcript_id")
    ) %>%
        as.data.frame() %>%
        set_colnames(c("geneID", "txID")) %>%
        # Put transcripts first
        .[, c("txID", "geneID")] %>%
        # Drop rows containing an NA value
        .[complete.cases(.), , drop = FALSE] %>%
        set_rownames(.[["txID"]])

    inform(paste(
        "tx2gene mappings:",
        length(unique(data[["txID"]])), "transcripts,",
        length(unique(data[["geneID"]])), "genes"
    ))

    data
}




# Aliases ======================================================================
#' @rdname tx2geneFromGFF
#' @export
tx2geneFromGFF -> tx2geneFromGTF
