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
#' x <- makeTx2geneFromGFF("http://basejump.seq.cloud/example.gtf")
#' glimpse(x)
makeTx2geneFromGFF <- function(file) {
    message("Making tx2gene from GFF")

    data <- readGFF(file) %>%
        as.data.frame() %>%
        camel() %>%
        select(!!!syms(c("transcriptID", "geneID"))) %>%
        unique() %>%
        arrange(!!sym("transcriptID")) %>%
        # Drop rows containing an NA value in either column
        .[complete.cases(.), , drop = FALSE] %>%
        set_rownames(.[["transcriptID"]])

    message(paste(
        "tx2gene mappings:",
        length(unique(data[["transcriptID"]])), "transcripts,",
        length(unique(data[["geneID"]])), "genes"
    ))

    data
}




# Aliases ======================================================================
#' @rdname makeTx2geneFromGFF
#' @usage NULL
#' @export
makeTx2geneFromGFF -> makeTx2geneFromGTF
