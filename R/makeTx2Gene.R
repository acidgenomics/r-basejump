#' Make Transcript-to-Gene Mappings
#'
#' @section GFF/GTF file:
#'
#' Remote URLs and compressed files are supported.
#'
#' @name makeTx2Gene
#' @include makeGRanges.R
#'
#' @inheritParams makeGRanges
#'
#' @seealso [makeGRanges].
#'
#' @return `Tx2Gene`.
#'
#' @examples
#' ## makeTx2GeneFromEnsembl ====
#' x <- makeTx2GeneFromEnsembl(organism = "Homo sapiens")
#' print(x)
#'
#' ## makeTx2GeneFromEnsDb ====
#' x <- makeTx2GeneFromEnsDb("EnsDb.Hsapiens.v75")
#' print(x)
#'
#' ## makeTx2GeneFromGFF ====
#' ## GTF
#' file <- file.path(basejumpCacheURL, "example.gtf")
#' x <- makeTx2GeneFromGFF(file)
#' print(x)
#'
#' ## GFF3
#' file <- file.path(basejumpCacheURL, "example.gff3")
#' x <- makeTx2GeneFromGFF(file)
#' print(x)
NULL



#' @rdname makeTx2Gene
#' @export
makeTx2GeneFromEnsembl <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall(args = list(level = "transcripts"))
        )
        Tx2Gene(gr)
    }
f <- formals(makeGRangesFromEnsembl)
f <- f[setdiff(names(f), c("level", "metadata", "..."))]
formals(makeTx2GeneFromEnsembl) <- f



#' @rdname makeTx2Gene
#' @export
makeTx2GeneFromEnsDb <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsDb,
            args = matchArgsToDoCall(args = list(level = "transcripts"))
        )
        Tx2Gene(gr)
    }
f <- formals(makeGRangesFromEnsDb)
f <- f[setdiff(names(f), "level")]
formals(makeTx2GeneFromEnsDb) <- f



#' @rdname makeTx2Gene
#' @export
makeTx2GeneFromGFF <- function(file) {
    message("Making Tx2Gene from GFF.")
    gff <- import(file)
    assert_is_all_of(gff, "GRanges")

    # Get information on the type of GFF file.
    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected."))

    # Coerce GRanges to tibble.
    data <- camel(as_tibble(gff))
    assert_is_subset("transcriptID", colnames(data))

    # Remove rows that don't contain transcript annotations.
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

    Tx2Gene(data)
}



# Aliases ======================================================================
#' @rdname makeTx2Gene
#' @usage NULL
#' @export
makeTx2GeneFromGTF <- makeTx2GeneFromGFF
