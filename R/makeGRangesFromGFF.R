#' Make `GRanges` from a GFF/GTF file
#'
#' @details
#' Remote URLs and compressed files are supported.
#'
#' @section Recommendations:
#'
#' - **Use GTF over GFF3.** We recommend using a GTF file instead of a GFF3
#'   file, when possible. The file format is more compact and easier to parse.
#' - **Use Ensembl over RefSeq.** We generally recommend using Ensembl over
#'   RefSeq, if possible. It's better supported in R and generally used by most
#'   NGS vendors.
#'
#' @section GFF/GTF specification:
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines.
#'
#' The GTF (General Transfer Format) format is identical to GFF version 2.
#'
#' The UCSC website has detailed conventions on the GFF3 format, including
#' the metadata columns.
#'
#' @section Supported sources:
#'
#' Currently [makeGRangesFromGFF()] supports genomes from these sources:
#'
#' - Ensembl: GTF, GFF3.
#' - GENCODE: GTF, GFF3.
#' - RefSeq: GFF3.
#' - FlyBase: GTF. GFF3 not supported.
#' - WormBase: GTF. GFF3 not supported.
#'
#' UCSC support will be added in a future release.
#'
#' @section Ensembl:
#'
#' Note that [makeGRangesFromEnsembl()] offers native support for Ensembl genome
#' builds and returns additional useful metadata that isn't defined inside a
#' GFF/GTF file.
#'
#' If you must load a GFF/GTF file directly, then use [makeGRangesFromGFF()].
#'
#' @section GENCODE vs. Ensembl:
#'
#' Annotations available from Ensembl and GENCODE are very similar.
#'
#' The GENCODE annotation is made by merging the manual gene annotation produced
#' by the Ensembl-Havana team and the Ensembl-genebuild automated gene
#' annotation. The GENCODE annotation is the default gene annotation displayed
#' in the Ensembl browser. The GENCODE releases coincide with the Ensembl
#' releases, although GENCODE can skip an Ensembl release if there is no update
#' to the annotation with respect to the previous release. In practical terms,
#' the GENCODE annotation is essentially identical to the Ensembl annotation.
#'
#' However, GENCODE handles pseudoautosomal regions (PAR) differently than
#' Ensembl. The Ensembl GTF file only includes this annotation once, for
#' chromosome X. However, GENCODE GTF/GFF3 files include the annotation in the
#' PAR regions of both chromosomes.
#'
#' The [GENCODE FAQ](https://www.gencodegenes.org/pages/faq.html) has additional
#' details.
#'
#' @section RefSeq:
#'
#' Refer to the
#' [current RefSeq GFF3 spec](ftp://ftp.ncbi.nlm.nih.gov/genomes/README_GFF3.txt)
#' for details.
#'
#' See also:
#'
#' - [RefSeq FAQ](https://www.ncbi.nlm.nih.gov/books/NBK50679/)
#' - ftp://ftp.ncbi.nih.gov/gene/DATA/gene2refseq.gz
#'
#' @section UCSC:
#'
#' Loading UCSC genome annotations from a GFF/GTF file are
#' *intentionally not supported* by this function.
#'
#' We recommend using a pre-built `TxDb` package from Bioconductor instead.
#' For example, load `TxDb.Hsapiens.UCSC.hg38.knownGene` for hg38.
#'
#' For reference, note that UCSC doesn't provide direct GFF/GTF file downloads.
#' Use of the [hgTables](https://genome.ucsc.edu/cgi-bin/hgTables) table
#' browser is required in a web browser.
#'
#' Select the following options to download hg38:
#'
#' - clade: Mammal
#' - genome: Human
#' - assembly: Dec. 2013 (GRCh38/hg38)
#' - group: Genes and Gene Predictions
#' - track: GENCODE v29
#' - table: knownGene
#' - region: genome
#' - output format: GTF - gene transfer format
#' - output file: Enter a file name
#'
#' See also:
#'
#' - http://genome.ucsc.edu/cgi-bin/hgTables
#' - http://hgdownload.soe.ucsc.edu/downloads.html
#' - ftp://hgdownload.soe.ucsc.edu/goldenPath/hg38/
#'
#' @section Example URLs:
#'
#' - Ensembl GTF:\cr
#'   ftp://ftp.ensembl.org/pub/release-95/gtf/homo_sapiens/Homo_sapiens.GRCh38.95.gtf.gz
#' - Ensembl GFF:\cr
#'   ftp://ftp.ensembl.org/pub/release-95/gff3/homo_sapiens/Homo_sapiens.GRCh38.95.gff3.gz
#' - GENCODE GTF:\cr
#'   ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_29/gencode.v29.annotation.gtf.gz
#' - GENCODE GFF:\cr
#'   ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_29/gencode.v29.annotation.gff3.gz
#' - RefSeq GFF:\cr
#'   ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/vertebrate_mammalian/Homo_sapiens/reference/GCF_000001405.38_GRCh38.p12/GCF_000001405.38_GRCh38.p12_genomic.gff.gz
#' - WormBase GTF:\cr
#'   ftp://ftp.wormbase.org/pub/wormbase/releases/WS268/species/c_elegans/PRJNA13758/c_elegans.PRJNA13758.WS268.canonical_geneset.gtf.gz
#' - FlyBase GTF:\cr
#'   ftp://ftp.flybase.net/releases/FB2018_05/dmel_r6.24/gtf/dmel-all-r6.24.gtf.gz
#'
#' @export
#' @inheritParams params
#'
#' @param strict `logical(1)`.
#'   Strict mode. Generate an internal `TxDb` using
#'   [GenomicFeatures::makeTxDbFromGRanges()] and check that the
#'   [`ranges()`][IRanges::ranges], [`seqnames()`][GenomeInfoDb::seqnames], and
#'   identifiers defined in [`names()`][base::names] are identical. Doesn't work
#'   for all GFF/GTF files due to some current limitations in the
#'   GenomicFeatures package, so this is disabled by default. Generally,
#'   GenomicFeatures parses GTF files better than GFF files. However, it's a
#'   useful sanity check and should be enabled if possible.
#'
#' @seealso
#' - [rtracklayer::import()].
#' - [GenomicFeatures::makeTxDbFromGRanges()].
#' - [GenomicFeatures::makeTxDbFromGFF()].
#'
#' @examples
#' file <- system.file("extdata/ensembl.gtf.gz", package = "basejump")
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
    level = c("genes", "transcripts"),
    strict = FALSE
) {
    message("Making GRanges from GFF file.")
    # Note that `import()` step has assert checks for file (see below).
    level <- match.arg(level)

    # Import -------------------------------------------------------------------
    # This step uses `rtracklayer::import()` internally.
    gff <- import(file)
    assert(is(gff, "GRanges"))

    source <- .detectGFFSource(gff)
    type <- .detectGFFType(gff)
    message(paste(source, type, "detected."))

    # Pre-flight checks --------------------------------------------------------
    # Not currently allowing FlyBase or WormBase GFF files. They're too
    # complicated to parse, and the sites offer GTF files instead anyway.
    if (
        source %in% c("FlyBase", "WormBase") &&
        type != "GTF"
    ) {
        stop(paste0(
            "Only GTF files are currently supported from ", source, "."
        ))
    }

    # TxDb (GenomicFeatures) ---------------------------------------------------
    # We're now using the TxDb object for GRanges strict mode sanity checks.
    #
    # Note that `makeTxDbFromGRanges()` frequently returns warnings.
    #
    # TxDb doesn't return enough useful metadata from the original GFF/GTF file,
    # so we're parsing the return manually here using custom code instead.
    #
    # Note that this step must be called before we attempt to sanitize any
    # metadata columns in `mcols()`.
    #
    # `makeTxDbFromGFF()` drops orphan exons/CDS in Ensembl GFF3:
    # - The following orphan exon were dropped
    # - The following orphan CDS were dropped
    #
    # `makeTxDbFromGRanges()` currently errors on Ensembl GFF3:
    # some exons are linked to transcripts not found in the file
    if (isTRUE(strict)) {
        message("Strict mode enabled.")
        message("Making TxDb using GenomicFeatures::makeTxDbFromGRanges().")
        txdb <- withCallingHandlers(expr = {
            # Using `tryCatch()` here to change error message, if necessary.
            tryCatch(
                expr = makeTxDbFromGRanges(gff),
                error = function(e) {
                    stop(paste0(
                        "Failed to make TxDb from GRanges using ",
                        "GenomicFeatures::makeTxDbFromGRanges().\n",
                        "Set `strict = FALSE` to disable the TxDb checks."
                    ), call. = FALSE)
                }
            )
        }, warning = function(w) {
            # Specifically suppressing this expected warning:
            # The "phase" metadata column contains non-NA values for
            # features of type stop_codon. This information was ignored.
            # See also:
            # - https://stackoverflow.com/questions/38603668
            # - https://stackoverflow.com/questions/16517795
            if (grepl(pattern = "stop_codon", x = conditionMessage(w))) {
                invokeRestart("muffleWarning")
            }
        })
        assert(is(txdb, "TxDb"))
    }

    # Standardization ----------------------------------------------------------
    if (source == "FlyBase" && type == "GTF") {
        # Rename `gene_symbol`, `transcript_symbol` to use `*_name` instead,
        # matching Ensembl GTF spec.
        colnames(mcols(gff)) <- gsub(
            pattern = "_symbol$",
            replacement = "_name",
            x = colnames(mcols(gff))
        )
    } else if (source == "WormBase" && type == "GTF") {
    } else if (source == "RefSeq") {
        stop("RefSeq isn't supported yet. Working on add this.")
    }

    # Always require `gene_id` and `transcript_id` columns in file.
    assert(isSubset(c("gene_id", "transcript_id"), colnames(mcols(gff))))

    # Genes --------------------------------------------------------------------
    # This function always returns gene-level metadata, even when transcripts
    # are requested. We'll merge this into the transcript-level GRanges below,
    # if necessary. Note that empty columns in `mcols()` will get dropped
    # automatically in the final `.makeGRanges()` return call.
    gn <- gff
    if (type == "GTF") {
        # This step is easy for GTF. Simply subset using the `type` column.
        gn <- gn[mcols(gn)[["type"]] == "gene"]
    } else if (source == "Ensembl" && type == "GFF") {
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
    } else {
        stop("Unsupported file.")
    }

    # Ensure the ranges are sorted by gene identifier.
    assert(
        hasLength(gn),
        !any(is.na(mcols(gn)[["gene_id"]])),
        hasNoDuplicates(mcols(gn)[["gene_id"]])
    )
    names(gn) <- mcols(gn)[["gene_id"]]
    gn <- gn[sort(names(gn))]

    # Ensure that the ranges match GenomicFeatures output.
    if (isTRUE(strict)) {
        .checkGRangesAgainstTxDb(
            gr = gn,
            txdb = txdb,
            level = "genes"
        )
    }

    # WormBase identifier fix. WormBase GTF currently imports somewhat
    # malformed, and the gene identifiers require additional sanitization to
    # return correctly. Some garbage rows containing "Gene:" or "Transcript:"
    # will remain. We need to drop these before proceeding.
    if (
        source == "WormBase" && type == "GTF") {
        keep <- !grepl(pattern = ":", x = mcols(gn)[["gene_id"]])
        gn <- gn[keep]
    }

    # Return the number of genes.
    if (level == "genes") {
        message(paste(length(gn), "gene annotations detected."))
        gr <- gn
    }

    # Transcripts --------------------------------------------------------------
    if (level == "transcripts") {
        tx <- gff
        if (source == "FlyBase" && type == "GTF") {
            # Note that FlyBase uses non-standard transcript types.
            keep <- grepl(
                pattern = paste(c("^pseudogene$", "RNA$"), collapse = "|"),
                x = mcols(tx)[["type"]],
                ignore.case = TRUE
            )
            tx <- tx[keep]
        } else if (type == "GTF") {
            # Subset using the `type` column.
            tx <- tx[mcols(tx)[["type"]] == "transcript"]
        } else if (source == "Ensembl" && type == "GFF") {
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
        # Ensure the ranges are sorted by transcript identifier.
        assert(
            hasLength(tx),
            !any(is.na(mcols(tx)[["transcript_id"]])),
            hasNoDuplicates(mcols(tx)[["transcript_id"]])
        )
        names(tx) <- mcols(tx)[["transcript_id"]]
        tx <- tx[sort(names(tx))]

        # Ensure that the ranges match GenomicFeatures output.
        if (isTRUE(strict)) {
            .checkGRangesAgainstTxDb(
                gr = tx,
                txdb = txdb,
                level = "transcripts"
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
    # Using `tryCatch()` here to suppress detection error and return empty
    # character. This step should work for most genomes.
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

    # Return with same formatting conventions for `makeGRangesFromEnsembl()`.
    # Note that this returns with `mcols()` renamed in camel case.
    .makeGRanges(gr)
}



# Aliases ======================================================================
#' @describeIn makeGRangesFromGFF GTF file extension alias.
#'   Runs the same internal code as [makeGRangesFromGFF()].
#' @export
makeGRangesFromGTF <- makeGRangesFromGFF



# Internal GFF utilites ========================================================
# Check GRanges from GFF loaded with rtracklayer against GenomicFeatures TxDb.
# Consider adding support for exons, CDS, in a future update.
#
# Expected warnings/failures:
# - FlyBase genes from GTF: FBgn0013687. Visual inspection confirms that ranges
#   from GFF are correct, but TxDb reports 258 mismatches (those are incorrect).
.checkGRangesAgainstTxDb <- function(
    gr,
    txdb,
    level = c("genes", "transcripts")
) {
    level <- match.arg(level)
    fun <- get(level)
    assert(is.function(fun))
    message(paste("Checking", level, "in TxDb."))

    # Convert the TxDb to GRanges using either `genes()` or `transcripts()`.
    grTxDb <- fun(txdb)
    assert(is(grTxDb, "GRanges"))

    # `GenomicFeatures::transcripts()` returns numbers instead of correct
    # transcript IDs, so fix that before checks. Note that this return maps
    # the correct identifiers to `tx_name` column in `mcols()`.
    if (level == "transcripts") {
        assert(isSubset("tx_name", colnames(mcols(grTxDb))))
        names(grTxDb) <- mcols(grTxDb)[["tx_name"]]
    }
    assert(hasNames(grTxDb))
    # Now ensure the TxDb output is arranged by identifier.
    grTxDb <- grTxDb[sort(names(grTxDb))]

    # Length and names ---------------------------------------------------------
    # Warn if GenomicFeatures has dropped ranges from GFF.
    # This can happen for some GFF3 files and FlyBase GTF.
    if (length(gr) > length(grTxDb)) {
        assert(isSubset(names(grTxDb), names(gr)))
        n <- length(gr) - length(grTxDb)
        warning(paste0(
            "GenomicFeatures dropped ",
            sprintf(
                fmt = ngettext(
                    n = n,
                    msg1 = "%s identifier",
                    msg2 = "%s identifiers"
                ),
                n
            ),
            " from GFF to make TxDb.\n",
            "Missing in TxDb: ",
            toString(c(head(setdiff(names(gr), names(grTxDb))), "..."))
        ))
        # Subset the GRanges from GFF to match TxDb for additional checks.
        gr <- gr[names(grTxDb)]
    }
    assert(
        identical(length(gr), length(grTxDb)),
        identical(names(gr), names(grTxDb))
    )

    # Ranges and seqnames ------------------------------------------------------
    # Compare the ranges and inform the user about mismatches.
    # help(topic = "IPosRanges-comparison", package = "IRanges")
    # vignette(topic = "IRangesOverview", package = "IRanges")
    r1 <- ranges(gr)
    r2 <- ranges(grTxDb)
    diff <- r1 != r2
    if (any(diff)) {
        warning(paste(
            sum(diff, na.rm = TRUE),
            "range mismatches detected in TxDb."
        ))
        which <- head(which(diff), n = 10L)
        cat(
            "Showing GRanges mismatch comparison (first 10).",
            "",
            "(1) GFF, via rtracklayer::import():",
            printString(r1[which]),
            "",
            "(2) TxDb, via GenomicFeatures::makeTxDbFromGRanges():",
            printString(r2[which]),
            "",
            "If the ranges in (1) are incorrect, please file an issue here:",
            "https://github.com/steinbaugh/basejump/issues",
            "",
            "If the ranges in (2) are incorrect, please file an issue here:",
            "https://github.com/Bioconductor/GenomicFeatures/issues",
            sep = "\n"
        )
    }
    assert(identical(seqnames(gr), seqnames(grTxDb)))

    invisible(TRUE)
}



# Report the source of the gene annotations.
.detectGFFSource <- function(gff) {
    assert(is(gff, "GRanges"))
    if (
        any(grepl(
            pattern = "FlyBase",
            x = mcols(gff)[["source"]],
            ignore.case = FALSE
        ))
    ) {
        "FlyBase"
    } else if (
        any(grepl(
            pattern = "WormBase",
            x = mcols(gff)[["source"]],
            ignore.case = FALSE
        ))
    ) {
        "WormBase"
    } else if (
        any(grepl(
            pattern = "RefSeq",
            x = mcols(gff)[["source"]],
            ignore.case = FALSE
        ))
    ) {
        "RefSeq"
    } else if (
        # e.g. hg38_knownGene
        any(grepl(
            pattern = "_knownGene$",
            x = mcols(gff)[["source"]],
            ignore.case = FALSE
        ))
    ) {
        stop(paste(
            "UCSC is intentionally not supported.",
            "Use a pre-built TxDb package instead.",
            "(e.g. TxDb.Hsapiens.UCSC.hg38.knownGene)",
            sep = "\n"
        ))
    } else if (
        any(grepl(
            pattern = "ensembl|havana",
            x = mcols(gff)[["source"]],
            ignore.case = TRUE
        ))
    ) {
        "Ensembl"
    } else {
        stop(paste(
            "Failed to detect supported GFF/GTF source.",
            "Currently supported:",
            "  - Ensembl",
            "  - GENCODE",
            "  - RefSeq",
            "  - FlyBase",
            "  - WormBase",
            sep = "\n"
        ))
    }
}



# FIXME Improve this step to check against more columns.
# Determine if GFF or GTF.

# Ensembl GTF:
#  [1] "source"                   "type"
#  [3] "score"                    "phase"
#  [5] "gene_id"                  "gene_version"
#  [7] "gene_name"                "gene_source"
#  [9] "gene_biotype"             "transcript_id"
# [11] "transcript_version"       "transcript_name"
# [13] "transcript_source"        "transcript_biotype"
# [15] "tag"                      "transcript_support_level"
# [17] "exon_number"              "exon_id"
# [19] "exon_version"             "protein_id"
# [21] "protein_version"          "ccds_id"

# Ensembl GFF:
#  [1] "source"                   "type"
#  [3] "score"                    "phase"
#  [5] "ID"                       "Alias"
#  [7] "external_name"            "logic_name"
#  [9] "Name"                     "biotype"
# [11] "description"              "gene_id"
# [13] "version"                  "Parent"
# [15] "tag"                      "transcript_id"
# [17] "transcript_support_level" "constitutive"
# [19] "ensembl_end_phase"        "ensembl_phase"
# [21] "exon_id"                  "rank"
# [23] "protein_id"               "ccdsid"

# GENCODE GTF
#  [1] "source"                   "type"
#  [3] "score"                    "phase"
#  [5] "gene_id"                  "gene_type"
#  [7] "gene_name"                "level"
#  [9] "havana_gene"              "transcript_id"
# [11] "transcript_type"          "transcript_name"
# [13] "transcript_support_level" "tag"
# [15] "havana_transcript"        "exon_number"
# [17] "exon_id"                  "ont"
# [19] "protein_id"               "ccdsid"

# GENCODE GFF
#  [1] "source"                   "type"
#  [3] "score"                    "phase"
#  [5] "ID"                       "gene_id"
#  [7] "gene_type"                "gene_name"
#  [9] "level"                    "havana_gene"
# [11] "Parent"                   "transcript_id"
# [13] "transcript_type"          "transcript_name"
# [15] "transcript_support_level" "tag"
# [17] "havana_transcript"        "exon_number"
# [19] "exon_id"                  "ont"
# [21] "protein_id"               "ccdsid"

.detectGFFType <- function(gff) {
    assert(is(gff, "GRanges"))
    if (all(c("ID", "Name") %in% colnames(mcols(gff)))) {
        "GFF"
    } else {
        "GTF"
    }
}
