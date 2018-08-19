#' Transcript-to-Gene Mappings from GFF File
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' # GTF
#' x <- makeTx2geneFromGFF("http://basejump.seq.cloud/example.gtf")
#' glimpse(x)
#'
#' # GFFv3
#' x <- makeTx2geneFromGFF("http://basejump.seq.cloud/example.gff3")
#' glimpse(x)
makeTx2geneFromGFF <- function(file) {
    message("Making tx2gene from GFF")
    gff <- readGFF(file)

    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected"))

    data <- mcols(gff) %>%
        as.data.frame() %>%
        camel()
    assert_is_subset("transcriptID", colnames(data))
    transcriptIDs <- sort(unique(na.omit(data[["transcriptID"]])))
    data <- filter(data, !is.na(!!sym("transcriptID")))

    if (type == "GFF") {
        assert_is_subset("parent", colnames(data))
        stopifnot(all(grepl("^gene:", data[["parent"]])))
        data[["geneID"]] <- as.character(data[["parent"]])
        data[["geneID"]] <- gsub(
            pattern = "^gene:",
            replacement = "",
            x = data[["geneID"]]
        )
    }

    data <- data %>%
        select(!!!syms(c("transcriptID", "geneID"))) %>%
        .[complete.cases(.), , drop = FALSE] %>%
        unique() %>%
        arrange(!!sym("transcriptID")) %>%
        set_rownames(.[["transcriptID"]])

    message(paste(
        "tx2gene mappings:",
        length(unique(data[["transcriptID"]])), "transcripts,",
        length(unique(data[["geneID"]])), "genes"
    ))

    assert_are_identical(transcriptIDs, rownames(data))
    data
}




#' @rdname makeTx2geneFromGFF
#' @usage NULL
#' @export
makeTx2geneFromGFF -> makeTx2geneFromGTF
