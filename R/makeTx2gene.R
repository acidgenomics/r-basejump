#' Make Transcript-to-Gene Mappings
#'
#' @name makeTx2gene
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams makeGRanges
#' @param ... Passthrough to [makeGRangesFromEnsembl()].
#'
#' @return `data.frame`.
#'
#' @seealso `help(topic = "makeGRanges", package = "basejump")`.
#'
#' @examples
#' # makeTx2geneFromEnsembl ====
#' x <- makeTx2geneFromEnsembl("Homo sapiens")
#' glimpse(x)
#'
#' # makeTx2geneFromGFF ====
#' # GTF
#' x <- makeTx2geneFromGFF("http://basejump.seq.cloud/example.gtf")
#' glimpse(x)
#'
#' # GFF3
#' x <- makeTx2geneFromGFF("http://basejump.seq.cloud/example.gff3")
#' glimpse(x)
NULL



#' @rdname makeTx2gene
#' @export
makeTx2geneFromEnsembl <- function(...) {
    gr <- makeGRangesFromEnsembl(..., format = "transcripts")
    mcols(gr) %>%
        as.data.frame() %>%
        select(!!!syms(c("transcriptID", "geneID"))) %>%
        mutate_all(as.character) %>%
        set_rownames(.[[1L]])
}



#' @rdname makeTx2gene
#' @export
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



#' @rdname makeTx2gene
#' @usage NULL
#' @export
makeTx2geneFromGFF -> makeTx2geneFromGTF
