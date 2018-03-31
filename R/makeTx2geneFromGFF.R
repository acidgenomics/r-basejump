#' Transcript-to-Gene Mappings from GFF File
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
#' makeTx2geneFromGFF("http://basejump.seq.cloud/mmusculus.gtf") %>%
#'     glimpse()
makeTx2geneFromGFF <- function(file) {
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
        .[order(.[["txID"]]), , drop = FALSE] %>%
        set_rownames(.[["txID"]])

    inform(paste(
        "tx2gene mappings:",
        length(unique(data[["txID"]])), "transcripts,",
        length(unique(data[["geneID"]])), "genes"
    ))

    data
}




# Aliases ======================================================================
#' @rdname makeTx2geneFromGFF
#' @usage NULL
#' @export
makeTx2geneFromGFF -> makeTx2geneFromGTF
