#' Make Transcript-to-Gene Mappings
#'
#' @section GFF/GTF file:
#'
#' Remote URLs and compressed files are supported.
#'
#' @name makeTx2gene
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @include makeGRanges.R
#'
#' @inheritParams makeGRanges
#'
#' @seealso [makeGRanges].
#'
#' @return `tx2gene`.
#'
#' @examples
#' # makeTx2geneFromEnsembl ====
#' x <- makeTx2geneFromEnsembl(organism = "Homo sapiens")
#' print(x)
#'
#' # makeTx2geneFromGFF ====
#' # GTF
#' file <- file.path(basejumpCacheURL, "example.gtf")
#' x <- makeTx2geneFromGFF(file)
#' print(x)
#'
#' # GFF3
#' file <- file.path(basejumpCacheURL, "example.gff3")
#' x <- makeTx2geneFromGFF(file)
#' print(x)
NULL



.makeTx2gene <- function(data) {
    data <- data %>%
        as_tibble() %>%
        select(!!!syms(c("transcriptID", "geneID"))) %>%
        .[complete.cases(.), , drop = FALSE] %>%
        unique() %>%
        mutate_all(as.character) %>%
        as("DataFrame") %>%
        set_rownames(.[["transcriptID"]])
    message(paste(
        "tx2gene mappings:",
        length(unique(data[["transcriptID"]])), "transcripts,",
        length(unique(data[["geneID"]])), "genes"
    ))
    new("tx2gene", data)
}



#' @rdname makeTx2gene
#' @export
makeTx2geneFromEnsembl <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall(args = list(level = "transcripts"))
        )
        .makeTx2gene(gr)
    }
f <- formals(makeGRangesFromEnsembl)
f <- f[setdiff(names(f), c("level", "metadata", "..."))]
formals(makeTx2geneFromEnsembl) <- f



#' @rdname makeTx2gene
#' @export
makeTx2geneFromGFF <- function(file) {
    message("Making tx2gene from GFF...")
    gff <- import(file)
    assert_is_all_of(gff, "GRanges")

    # Get information on the type of GFF file.
    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected."))

    # Coerce GRanges to tibble.
    data <- camel(as(gff, "tbl_df"))
    assert_is_subset("transcriptID", colnames(data))

    # Remove rows that don't contain transcript annotations.
    data <- filter(data, !is.na(!!sym("transcriptID")))

    # Get a vector of unique transcript IDs.
    transcriptIDs <- unique(data[["transcriptID"]])

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



# Aliases ======================================================================
#' @rdname makeTx2gene
#' @export
makeTx2geneFromGTF <- makeTx2geneFromGFF
