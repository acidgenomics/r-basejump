#' Make `GRanges` from GFF/GTF file
#'
#' @description
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2. We recommend
#' using a GTF file instead of a GFF3 file, if possible.
#'
#' The UCSC website has detailed conventions on the GFF3 format, including
#' the metadata columns.
#'
#' Remote URLs and compressed files are supported.
#'
#' @inheritParams params
#' @export
#'
#' @seealso
#' - `rtracklayer::import()`.
#' - `GenomicFeatures::makeTxDbFromGFF()`.
#'
#' @examples
#' file <- file.path(basejumpCacheURL, "example.gtf")
#'
#' ## Genes
#' x <- makeGRangesFromGFF(file = file, level = "genes")
#' summary(x)
#'
#' ## Transcripts
#' x <- makeGRangesFromGFF(file = file, level = "transcripts")
#' summary(x)
makeGRangesFromGFF <- function(
    file,
    level = c("genes", "transcripts")
) {
    message("Making GRanges from GFF/GTF file.")
    # Note that `import` has assert checks for file (see below).
    level <- match.arg(level)

    # Import -------------------------------------------------------------------
    file <- localOrRemoteFile(file)
    gff <- import(file)
    assert(is(gff, "GRanges"))
    gff <- camel(gff)

    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected."))

    # nocov start
    if (
        source %in% c("FlyBase", "WormBase") &&
        type == "GFF"
    ) {
        stop(paste0(
            "Only GTF files are currently supported from ", source, "."
        ))
    }
    # nocov end

    # Always require `geneID` and `transcriptID` columns in file.
    assert(isSubset(x = c("geneID", "transcriptID"), y = colnames(mcols(gff))))

    # Rename `geneSymbol` to `geneName`.
    # This applies to FlyBase and WormBase annotations.
    colnames(mcols(gff)) <- gsub(
        pattern = "Symbol$",
        replacement = "Name",
        x = colnames(mcols(gff))
    )

    # Genes --------------------------------------------------------------------
    gn <- gff
    gn <- gn[!is.na(mcols(gn)[["geneID"]])]
    gn <- gn[is.na(mcols(gn)[["transcriptID"]])]
    if (type == "GFF") {
        # geneName
        assert(isSubset("name", colnames(mcols(gn))))
        mcols(gn)[["geneName"]] <- mcols(gn)[["name"]]
        mcols(gn)[["name"]] <- NULL
        # geneBiotype
        assert(isSubset("biotype", colnames(mcols(gn))))
        mcols(gn)[["geneBiotype"]] <- mcols(gn)[["biotype"]]
        mcols(gn)[["biotype"]] <- NULL
        # Remove extra columns.
        mcols(gn)[["alias"]] <- NULL
        mcols(gn)[["id"]] <- NULL
        mcols(gn)[["parent"]] <- NULL
    }
    assert(hasNoDuplicates(mcols(gn)[["geneID"]]))
    names(gn) <- mcols(gn)[["geneID"]]
    gn <- gn[sort(names(gn))]

    # Stop on missing genes.
    assert(identical(
        x = names(gn),
        y = sort(unique(na.omit(mcols(gff)[["geneID"]])))
    ))

    if (level == "genes") {
        message(paste(length(gn), "gene annotations detected."))
        gr <- gn
    }

    # Transcripts --------------------------------------------------------------
    if (level == "transcripts") {
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
            assert(isSubset("name", colnames(mcols(tx))))
            mcols(tx)[["transcriptName"]] <- mcols(tx)[["name"]]
            mcols(tx)[["name"]] <- NULL
            # transcriptBiotype
            assert(isSubset("biotype", colnames(mcols(tx))))
            mcols(tx)[["transcriptBiotype"]] <- mcols(tx)[["biotype"]]
            mcols(tx)[["biotype"]] <- NULL
            # geneID
            assert(
                isSubset("parent", colnames(mcols(tx))),
                all(grepl("^gene:", mcols(tx)[["parent"]]))
            )
            mcols(tx)[["geneID"]] <- as.character(mcols(tx)[["parent"]])
            mcols(tx)[["geneID"]] <- gsub(
                pattern = "^gene:",
                replacement = "",
                x = mcols(tx)[["geneID"]]
            )
            # Remove extra columns.
            mcols(tx)[["alias"]] <- NULL
            mcols(tx)[["id"]] <- NULL
            mcols(tx)[["parent"]] <- NULL
        }
        assert(hasNoDuplicates(mcols(tx)[["transcriptID"]]))
        names(tx) <- mcols(tx)[["transcriptID"]]
        tx <- tx[sort(names(tx))]

        # Stop on missing transcripts.
        assert(identical(
            x = names(tx),
            y = sort(unique(na.omit(mcols(gff)[["transcriptID"]])))
        ))

        message(paste(length(tx), "transcript annotations detected."))
        gr <- tx

        # Merge the gene-level annotations (`geneName`, `geneBiotype`).
        geneCols <- setdiff(
            x = colnames(mcols(gn)),
            y = colnames(mcols(gr))
        )
        if (length(geneCols) > 0L) {
            geneCols <- c("geneID", geneCols)
            merge <- merge(
                x = mcols(gr),
                y = mcols(gn)[, geneCols],
                all.x = TRUE,
                by = "geneID"
            )
            rownames(merge) <- merge[["transcriptID"]]
            merge <- merge[sort(rownames(merge)), ]
            assert(identical(
                x = mcols(gr)[["transcriptID"]],
                y = merge[["transcriptID"]]
            ))
            mcols(gr) <- merge
        }
    }

    # Metadata -----------------------------------------------------------------
    organism <- tryCatch(
        expr = organism(gr),
        error = function(e) character()
    )
    metadata(gr) <- c(
        .prototypeMetadata,
        list(
            file = file,
            level = level,
            organism = organism,
            genomeBuild = character(),
            ensemblRelease = integer()
        )
    )

    .makeGRanges(gr)
}



#' @describeIn makeGRangesFromGFF File format alias.
#'   Runs the same internal code as [makeGRangesFromGFF()].
#' @export
makeGRangesFromGTF <- makeGRangesFromGFF
