# TODO UCSC Support
# TODO FlyBase GFF support
# TODO WormBase GFF support
# TODO WormBase has some malformed entries in GTF: Transcript:AC8.13
# We may want to add a filter for these



#' Genomic Ranges from GFF File
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' The UCSC website has detailed conventions on the GFF3 format, including
#' the metadata columns.
#'
#' @family Gene Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param format Output as genes or transcripts.
#'
#' @return `GRanges`.
#' @export
#'
#' @seealso
#' - [rtracklayer::import()].
#' - [GenomicFeatures::makeTxDbFromGFF()].
#'
#' @examples
#' file <- "http://basejump.seq.cloud/example.gtf"
#'
#' # Genes
#' x <- makeGRangesFromGFF(file = file, format = "genes")
#' summary(x)
#' as.data.frame(x) %>% glimpse()
#'
#' # Transcripts
#' x <- makeGRangesFromGFF(file = file, format = "transcripts")
#' summary(x)
#' as.data.frame(x) %>% glimpse()
makeGRangesFromGFF <- function(
    file,
    format = c("genes", "transcripts")
) {
    file <- localOrRemoteFile(file)
    # Require GFF or GTF file extension
    stopifnot(grepl("\\.g[ft]f", file, ignore.case = TRUE))
    format <- match.arg(format)

    # Import GFF as GRanges (using rtracklayer)
    gff <- readGFF(file)
    assert_is_all_of(gff, "GRanges")
    gff <- camel(gff)

    # Determine if GFF or GTF
    if (all(c("id", "name") %in% colnames(mcols(gff)))) {
        type <- "GFF"
    } else {
        type <- "GTF"
    }

    # Report the source of the gene annotations
    assert_is_subset("source", colnames(mcols(gff)))
    if (any(grepl("FlyBase", mcols(gff)[["source"]]))) {
        source <- "FlyBase"
    } else if (any(grepl("WormBase", mcols(gff)[["source"]]))) {
        source <- "WormBase"
    } else if (any(grepl(
        "ensembl", mcols(gff)[["source"]], ignore.case = TRUE
    ))) {
        source <- "Ensembl"
    } else {
        stop("Unsupported GFF source")
    }

    message(paste(source, type, "detected"))

    if (
        source %in% c("FlyBase", "WormBase") &&
        type == "GFF"
    ) {
        stop(paste(
            "Only GTF files are currently supported from", source
        ))
    }

    # Always require `geneID` and `transcriptID` columns in file
    assert_is_subset(
        x = c("geneID", "transcriptID"),
        y = colnames(mcols(gff))
    )

    # Rename `geneSymbol` to `geneName`.
    # This applies to FlyBase and WormBase annotations
    colnames(mcols(gff)) <- gsub(
        pattern = "Symbol$",
        replacement = "Name",
        x = colnames(mcols(gff))
    )

    # Genes ====================================================================
    gn <- gff
    gn <- gn[!is.na(mcols(gn)[["geneID"]])]
    gn <- gn[is.na(mcols(gn)[["transcriptID"]])]
    if (type == "GFF") {
        # geneName
        assert_is_subset("name", colnames(mcols(gn)))
        mcols(gn)[["geneName"]] <- mcols(gn)[["name"]]
        mcols(gn)[["name"]] <- NULL
        # geneBiotype
        assert_is_subset("biotype", colnames(mcols(gn)))
        mcols(gn)[["geneBiotype"]] <- mcols(gn)[["biotype"]]
        mcols(gn)[["biotype"]] <- NULL
        # Remove extra columns
        mcols(gn)[["alias"]] <- NULL
        mcols(gn)[["id"]] <- NULL
        mcols(gn)[["parent"]] <- NULL
    }
    assert_has_no_duplicates(mcols(gn)[["geneID"]])
    names(gn) <- mcols(gn)[["geneID"]]
    gn <- gn[sort(names(gn))]

    # Stop on missing genes
    assert_are_identical(
        x = names(gn),
        y = sort(unique(na.omit(mcols(gff)[["geneID"]])))
    )

    if (format == "genes") {
        message(paste(length(gn), "gene annotations"))
        gr <- gn
    }

    # Transcripts ==============================================================
    if (format == "transcripts") {
        tx <- gff
        tx <- tx[!is.na(mcols(tx)[["transcriptID"]])]
        if (type == "GTF") {
            types <- c(
                "pseudogene",
                "rna",
                "transcript"
            )
            tx <- tx[grepl(
                pattern = paste(types, collapse = "|"),
                x = mcols(tx)[["type"]],
                ignore.case = TRUE
            )]
        } else if (type == "GFF") {
            # transcriptName
            assert_is_subset("name", colnames(mcols(tx)))
            mcols(tx)[["transcriptName"]] <- mcols(tx)[["name"]]
            mcols(tx)[["name"]] <- NULL
            # transcriptBiotype
            assert_is_subset("biotype", colnames(mcols(tx)))
            mcols(tx)[["transcriptBiotype"]] <- mcols(tx)[["biotype"]]
            mcols(tx)[["biotype"]] <- NULL
            # geneID
            assert_is_subset("parent", colnames(mcols(tx)))
            stopifnot(all(grepl("^gene:", mcols(tx)[["parent"]])))
            mcols(tx)[["geneID"]] <- as.character(mcols(tx)[["parent"]])
            mcols(tx)[["geneID"]] <- gsub(
                pattern = "^gene:",
                replacement = "",
                x = mcols(tx)[["geneID"]]
            )
            # Remove extra columns
            mcols(tx)[["alias"]] <- NULL
            mcols(tx)[["id"]] <- NULL
            mcols(tx)[["parent"]] <- NULL
        }
        assert_has_no_duplicates(mcols(tx)[["transcriptID"]])
        names(tx) <- mcols(tx)[["transcriptID"]]
        tx <- tx[sort(names(tx))]

        # Stop on missing transcripts
        assert_are_identical(
            x = names(tx),
            y = sort(unique(na.omit(mcols(gff)[["transcriptID"]])))
        )

        message(paste(length(tx), "transcript annotations"))
        gr <- tx

        # Merge the gene-level annotations (`geneName`, `geneBiotype`)
        geneCols <- setdiff(
            x = colnames(mcols(gn)),
            y = colnames(mcols(gr))
        )
        if (length(geneCols)) {
            geneCols <- c("geneID", geneCols)
            merge <- merge(
                x = mcols(gr),
                y = mcols(gn)[, geneCols],
                all.x = TRUE,
                by = "geneID"
            )
            rownames(merge) <- merge[["transcriptID"]]
            merge <- merge[sort(rownames(merge)), ]
            assert_are_identical(
                x = mcols(gr)[["transcriptID"]],
                y = merge[["transcriptID"]]
            )
            mcols(gr) <- merge
        }
    }

    .makeGRanges(gr)
}



# Aliases ======================================================================
#' @rdname makeGRangesFromGFF
#' @usage NULL
#' @export
makeGRangesFromGFF -> makeGRangesFromGTF
