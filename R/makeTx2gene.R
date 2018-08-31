# FIXME Improve the formals here.



#' Make Transcript-to-Gene Mappings
#'
#' @name makeTx2gene
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams makeGRanges
#' @param ... Passthrough to [makeGRangesFromEnsembl()].
#'
#' @return `DataFrame`.
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



.makeTx2gene <- function(data) {
    data <- data %>%
        as("tbl_df") %>%
        select(!!!syms(c("transcriptID", "geneID"))) %>%
        .[complete.cases(.), , drop = FALSE] %>%
        unique() %>%
        mutate_all(as.character) %>%
        arrange(!!sym("transcriptID")) %>%
        as("DataFrame") %>%
        set_rownames(.[["transcriptID"]])
    message(paste(
        "tx2gene mappings:",
        length(unique(data[["transcriptID"]])), "transcripts,",
        length(unique(data[["geneID"]])), "genes"
    ))
    data
}



#' @rdname makeTx2gene
#' @export
makeTx2geneFromEnsembl <- function(...) {
    gr <- makeGRangesFromEnsembl(..., format = "transcripts")
    data <- .makeTx2gene(gr)
    assert_are_identical(names(gr), rownames(data))
    data
}



#' @rdname makeTx2gene
#' @export
makeTx2geneFromGFF <- function(file) {
    message("Making tx2gene from GFF")
    gff <- readGFF(file)
    assert_is_all_of(gff, "GRanges")

    # Get information on the type of GFF file.
    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected"))

    # Coerce GRanges to tibble.
    data <- camel(as(gff, "tbl_df"))
    assert_is_subset("transcriptID", colnames(data))

    # Remove rows that don't contain transcript annotations.
    data <- filter(data, !is.na(!!sym("transcriptID")))

    # Get a vector of unique transcript IDs.
    transcriptIDs <- sort(unique(data[["transcriptID"]]))

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

    data <- .makeTx2gene(data)
    assert_are_identical(transcriptIDs, rownames(data))
    data
}



#' @rdname makeTx2gene
#' @usage NULL
#' @export
makeTx2geneFromGFF -> makeTx2geneFromGTF
