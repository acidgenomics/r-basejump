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
#' @section Supported sources:
#'
#' Currently [makeGRangesFromGFF()] supports these sources:
#'
#' - Ensembl (GTF, GFF3)
#' - GENCODE, which inherits from Ensembl
#' - RefSeq (GFF3)
#' - FlyBase (GTF only)
#' - WormBase (GTF only)
#'
#' @section Commonly used GFF/GTF files:
#'
#' - Ensembl GTF:\cr
#'   ftp://ftp.ensembl.org/pub/release-95/gtf/homo_sapiens/Homo_sapiens.GRCh38.95.gtf.gz
#' - Ensembl GFF3:\cr
#'   ftp://ftp.ensembl.org/pub/release-95/gff3/homo_sapiens/Homo_sapiens.GRCh38.95.gff3.gz
#' - RefSeq:\cr
#'   ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/vertebrate_mammalian/Homo_sapiens/reference/GCF_000001405.38_GRCh38.p12/GCF_000001405.38_GRCh38.p12_genomic.gff.gz
#'
#' @section Metadata column names:
#'
#' ## Ensembl GTF (preferred)
#'
#' - source
#' - type
#' - score
#' - phase
#' - gene_id
#' - gene_version
#' - gene_name
#' - gene_source
#' - gene_biotype
#' - transcript_id
#' - transcript_version
#' - transcript_name
#' - transcript_source
#' - transcript_biotype
#' - tag
#' - transcript_support_level
#' - exon_number
#' - exon_id
#' - exon_version
#' - protein_id
#' - protein_version
#' - ccds_id
#'
#' ## Ensembl GFF3
#'
#' - "source"
#' - "type"
#' - "score"
#' - "phase"
#' - "ID"
#' - "Alias"
#' - "external_name"
#' - "logic_name"
#' - "Name"
#' - "biotype"
#' - "description"
#' - "gene_id"
#' - "version"
#' - "Parent"
#' - "tag"
#' - "transcript_id"
#' - "transcript_support_level"
#' - "constitutive"
#' - "ensembl_end_phase"
#' - "ensembl_phase"
#' - "exon_id"
#' - "rank"
#' - "protein_id"
#' - "ccdsid"
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

    # Import (rtracklayer) -----------------------------------------------------
    file <- localOrRemoteFile(file)
    gff <- import(file)
    assert(is(gff, "GRanges"))

    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected."))

    # Pre-flight checks --------------------------------------------------------
    # nocov start
    # Not currently allowing FlyBase or WormBase GFF files.
    # They're too complicated to parse, and they offer GTF files instead anyway.
    if (
        source %in% c("FlyBase", "WormBase") &&
        type == "GFF"
    ) {
        stop(paste0(
            "Only GTF files are currently supported from ", source, "."
        ))
    }
    # nocov end

    # TxDb (GenomicFeatures) ---------------------------------------------------
    # We're now using the TxDb object for GRanges sanity checks.
    #
    # TxDb doesn't return enough useful metadata from the original GFF/GTF file,
    # so we're parsing the return manually here instead.
    #
    # Note that this step must be called before we attempt to sanitize any
    # metadata columns in `mcols()`.
    #
    # Expecting this warning, which is safe to suppress:
    # The "phase" metadata column contains non-NA values for
    # features of type stop_codon. This information was ignored.
    #
    # `makeTxDbFromGFF()` drops orphan exons/CDS in Ensembl GFF3:
    # - The following orphan exon were dropped
    # - The following orphan CDS were dropped
    #
    # `makeTxDbFromGRanges()` currently errors on Ensembl GFF3:
    # some exons are linked to transcripts not found in the file
    message("Making TxDb using GenomicFeatures::makeTxDbFromGRanges().")
    txdb <- tryCatch(
        expr = suppressWarnings(makeTxDbFromGRanges(gff)),
        error = function(e) NULL
    )
    if (!is(txdb, "TxDb")) {
        warning(paste(
            "GenomicFeatures failed to make TxDb from GRanges.",
            "Skipping downstream checks using TxDb.",
            sep = "\n"
        ))
    }

    # Standardization ----------------------------------------------------------
    # Rename `gene_symbol` to `gene_name`, if necessary.
    # This applies to FlyBase and WormBase annotations.
    if (source %in% c("FlyBase", "WormBase")) {
        colnames(mcols(gff)) <- gsub(
            pattern = "_symbol$",
            replacement = "_name",
            x = colnames(mcols(gff))
        )
    }

    # RefSeq GFF files require column name sanitization.
    if (source == "RefSeq") {
        stop("NOT SUPPORTED YET")
        colnames(mcols(gff))
    }

    # Always require `gene_id` and `transcript_id` columns in file.
    assert(isSubset(
        x = c("gene_id", "transcript_id"),
        y = colnames(mcols(gff))
    ))

    # Genes --------------------------------------------------------------------
    # Note that this function always returns gene-level metadata, even when
    # transcripts are requested.
    gn <- gff
    gn <- gn[!is.na(mcols(gn)[["gene_id"]])]
    gn <- gn[is.na(mcols(gn)[["transcript_id"]])]
    if (type == "GFF") {
        # Assign `gene_name` column.
        assert(isSubset("Name", colnames(mcols(gn))))
        mcols(gn)[["gene_name"]] <- mcols(gn)[["Name"]]
        mcols(gn)[["Name"]] <- NULL
        # Assign `gene_biotype` column.
        assert(isSubset("biotype", colnames(mcols(gn))))
        mcols(gn)[["gene_biotype"]] <- mcols(gn)[["biotype"]]
        mcols(gn)[["biotype"]] <- NULL
        # Remove extra columns.
        mcols(gn)[["Alias"]] <- NULL
        mcols(gn)[["ID"]] <- NULL
        mcols(gn)[["Parent"]] <- NULL
    }
    assert(hasNoDuplicates(mcols(gn)[["gene_id"]]))
    names(gn) <- mcols(gn)[["gene_id"]]
    gn <- gn[sort(names(gn))]

    # Stop on missing genes.
    assert(identical(
        x = names(gn),
        y = sort(unique(na.omit(mcols(gff)[["gene_id"]])))
    ))

    # Ensure that the ranges match GenomicFeatures output.
    if (is(txdb, "TxDb")) {
        message("Checking gene metadata in TxDb.")
        gnFromTxDb <- genes(txdb)
        assert(
            identical(names(gn), names(gnFromTxDb)),
            identical(mcols(gn)[["gene_id"]], mcols(gnFromTxDb)[["gene_id"]]),
            identical(ranges(gn), ranges(gnFromTxDb)),
            identical(seqnames(gn), seqnames(gnFromTxDb))
        )
    }

    # Return the number of genes.
    if (level == "genes") {
        message(paste(length(gn), "gene annotations detected."))
        gr <- gn
    }

    # Transcripts --------------------------------------------------------------
    if (level == "transcripts") {
        tx <- gff
        tx <- tx[!is.na(mcols(tx)[["transcript_id"]])]
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
            # Assign `transcript_name`.
            assert(isSubset("Name", colnames(mcols(tx))))
            mcols(tx)[["transcript_name"]] <- mcols(tx)[["Name"]]
            mcols(tx)[["Name"]] <- NULL
            # Assign `transcript_biotype`.
            assert(isSubset("biotype", colnames(mcols(tx))))
            mcols(tx)[["transcript_biotype"]] <- mcols(tx)[["biotype"]]
            mcols(tx)[["biotype"]] <- NULL
            # Assign `gene_id` from `Parent`.
            assert(
                isSubset("Parent", colnames(mcols(tx))),
                all(grepl("^gene:", mcols(tx)[["Parent"]]))
            )
            mcols(tx)[["gene_id"]] <- as.character(mcols(tx)[["Parent"]])
            mcols(tx)[["gene_id"]] <- gsub(
                pattern = "^gene:",
                replacement = "",
                x = mcols(tx)[["gene_id"]]
            )
            # Remove extra columns.
            mcols(tx)[["Alias"]] <- NULL
            mcols(tx)[["ID"]] <- NULL
            mcols(tx)[["Parent"]] <- NULL
        }
        assert(hasNoDuplicates(mcols(tx)[["transcript_id"]]))
        names(tx) <- mcols(tx)[["transcript_id"]]
        tx <- tx[sort(names(tx))]

        # Stop on missing transcripts.
        assert(identical(
            x = names(tx),
            y = sort(unique(na.omit(mcols(gff)[["transcript_id"]])))
        ))

        # Ensure that the ranges match GenomicFeatures output.
        # Note that this currently returns unnamed, so we need to name and sort.
        if (is(txdb, "TxDb")) {
            message("Checking transcript metadata in TxDb.")
            txFromTxDb <- transcripts(txdb)
            names(txFromTxDb) <- mcols(txFromTxDb)[["tx_name"]]
            txFromTxDb <- txFromTxDb[sort(names(txFromTxDb))]
            assert(
                identical(names(tx), names(txFromTxDb)),
                identical(ranges(tx), ranges(txFromTxDb)),
                identical(seqnames(tx), seqnames(txFromTxDb))
            )
        }

        message(paste(length(tx), "transcript annotations detected."))
        gr <- tx

        # Merge the gene-level annotations (`geneName`, `geneBiotype`).
        geneCols <- setdiff(
            x = colnames(mcols(gn)),
            y = colnames(mcols(gr))
        )
        if (length(geneCols) > 0L) {
            geneCols <- c("gene_id", geneCols)
            merge <- merge(
                x = mcols(gr),
                y = mcols(gn)[, geneCols],
                all.x = TRUE,
                by = "gene_id"
            )
            rownames(merge) <- merge[["transcript_id"]]
            merge <- merge[sort(rownames(merge)), , drop = FALSE]
            assert(identical(
                x = mcols(gr)[["transcript_id"]],
                y = merge[["transcript_id"]]
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



#' @describeIn makeGRangesFromGFF GTF file extension alias.
#'   Runs the same internal code as [makeGRangesFromGFF()].
#' @export
makeGRangesFromGTF <- makeGRangesFromGFF
