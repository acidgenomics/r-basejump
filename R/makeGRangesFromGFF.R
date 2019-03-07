# nolint start

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
#' \tabular{lrrl}{
#'            \tab   GTF \tab  GFF3 \tab                                     \cr
#'   Ensembl  \tab  TRUE \tab  TRUE \tab Also available via ensembldb.       \cr
#'   GENCODE  \tab  TRUE \tab  TRUE \tab Similar to Ensembl; duplicate PAR_Y.\cr
#'   RefSeq   \tab    NA \tab  TRUE \tab Experimental support.               \cr
#'   UCSC     \tab FALSE \tab FALSE \tab Use pre-built Bioconductor packages.\cr
#'   FlyBase  \tab  TRUE \tab FALSE \tab Newer than Ensembl.                 \cr
#'   WormBase \tab  TRUE \tab FALSE \tab Very similar to Ensembl.            \cr
#' }
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
#' PAR regions of both chromosomes. You'll see these genes contain a "_PAR_Y"
#' suffix.
#'
#' Additionally, GENCODE GFF/GTF files import with a gene identifier containing
#' a suffix, which differs slightly from the Ensembl GFF/GTF spec
#' (e.g. GENCODE: ENSG00000000003.14; Ensembl: ENSG00000000003).
#'
#' The [GENCODE FAQ](https://www.gencodegenes.org/pages/faq.html) has additional
#' details.
#'
#' @section RefSeq:
#'
#' Refer to the
#' [current RefSeq spec](ftp://ftp.ncbi.nlm.nih.gov/genomes/README_GFF3.txt)
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
#' - clade: `Mammal`
#' - genome: `Human`
#' - assembly: `Dec. 2013 (GRCh38/hg38)`
#' - group: `Genes and Gene Predictions`
#' - track: `GENCODE v29`
#' - table: `knownGene`
#' - region: `genome`
#' - output format: `GTF - gene transfer format`
#' - output file: `<Enter a file name>`
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
#' - Ensembl GFF3:\cr
#'   ftp://ftp.ensembl.org/pub/release-95/gff3/homo_sapiens/Homo_sapiens.GRCh38.95.gff3.gz
#' - GENCODE GTF:\cr
#'   ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_29/gencode.v29.annotation.gtf.gz
#' - GENCODE GFF3:\cr
#'   ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_29/gencode.v29.annotation.gff3.gz
#' - RefSeq GFF3:\cr
#'   ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/vertebrate_mammalian/Homo_sapiens/reference/GCF_000001405.38_GRCh38.p12/GCF_000001405.38_GRCh38.p12_genomic.gff.gz
#' - FlyBase GTF:\cr
#'   ftp://ftp.flybase.net/releases/FB2018_05/dmel_r6.24/gtf/dmel-all-r6.24.gtf.gz
#' - WormBase GTF:\cr
#'   ftp://ftp.wormbase.org/pub/wormbase/releases/WS267/species/c_elegans/PRJNA13758/c_elegans.PRJNA13758.WS267.canonical_geneset.gtf.gz
#'
#' @export
#' @inheritParams params
#'
#' @param .checkAgainstTxDb `logical(1)`.
#'   Enable strict mode, intended for development and unit testing only.
#'   Generate an internal `TxDb` using [GenomicFeatures::makeTxDbFromGRanges()]
#'   and check that the [`ranges()`][IRanges::ranges],
#'   [`seqnames()`][GenomeInfoDb::seqnames], and identifiers defined in
#'   [`names()`][base::names] are identical. Doesn't work for all GFF/GTF files
#'   due to some current limitations in the GenomicFeatures package, so this is
#'   disabled by default. Generally, GenomicFeatures parses GTF files better
#'   than GFF files. However, it's a useful sanity check and should be enabled
#'   if possible.
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

# nolint end

makeGRangesFromGFF <- function(
    file,
    level = c("genes", "transcripts"),
    .checkAgainstTxDb = FALSE
) {
    assert(
        isString(file),
        isFlag(.checkAgainstTxDb)
    )
    level <- match.arg(level)
    message("Making GRanges from GFF file.")

    # Import -------------------------------------------------------------------
    # This step uses `rtracklayer::import()` internally.
    object <- import(file)
    # Slot the source (e.g. Ensembl) and type (e.g. GTF) into `metadata()`.
    object <- .slotGFFDetectInfo(object)
    # Pull detection strings from GRanges `metadata()`.
    detect <- metadata(object)[["detect"]]
    assert(is.character(detect))
    source <- detect[["source"]]
    type <- detect[["type"]]
    assert(isString(source), isString(type))

    # Pre-flight checks --------------------------------------------------------
    # Not currently allowing FlyBase or WormBase GFF files. They're too
    # complicated to parse, and the sites offer GTF files instead anyway.
    if (source %in% c("FlyBase", "WormBase") && type != "GTF") {
        stop(sprintf("Only GTF files from %s are supported.", source))
    }

    # TxDb (GenomicFeatures) ---------------------------------------------------
    # Run this step prior to any GRanges sanitization steps.
    if (isTRUE(.checkAgainstTxDb)) {
        message("Strict mode enabled. Checking against TxDb.")
        txdb <- .makeTxDbFromGFF(object)
    } else {
        txdb <- NULL
    }

    # Standardize --------------------------------------------------------------
    # Run this step after TxDb generation. Ensembl, GENCODE, and WormBase files
    # follow expected (Ensembl-like) naming conventions. Note that this step
    # needs to be run prior to `.makeGenesFromGFF()` and/or
    # `.makeTranscriptsFromGFF()` calls (see below).
    if (source == "FlyBase") {
        object <- .standardizeFlyBaseToEnsembl(object)
    } else if (source == "GENCODE") {
        object <- .standardizeGencodeToEnsembl(object)
    } else if (source == "RefSeq") {
        object <- .standardizeRefSeqToEnsembl(object)
    }
    mcolnames <- colnames(mcols(object))
    assert(
        isSubset(c("gene_id", "transcript_id"), mcolnames),
        # This should be renamed to `gene_biotype`, if defined.
        areDisjointSets("gene_type", mcolnames)
    )
    rm(mcolnames)

    # Genes --------------------------------------------------------------------
    # `makeGRangesFromGFF()` attempts to always returns gene-level metadata,
    # even when transcripts are requested. We'll merge this object into the
    # transcript-level GRanges below, when possible.
    genes <- object

    # Drop any rows that aren't gene specific.
    genes <- genes[!is.na(mcols(genes)[["gene_id"]])]
    genes <- genes[is.na(mcols(genes)[["transcript_id"]])]
    assert(hasLength(genes))

    if (source == "Ensembl" && type == "GFF3") {
        genes <- .makeGenesFromEnsemblGFF3(genes)
    } else if (source == "Ensembl" && type == "GTF") {
        genes <- .makeGenesFromEnsemblGTF(genes)
    } else if (source == "FlyBase" && type == "GTF") {
        genes <- .makeGenesFromFlyBaseGTF(genes)
    } else if (source == "GENCODE" && type == "GFF3") {
        genes <- .makeGenesFromGencodeGFF3(genes)
    } else if (source == "GENCODE" && type == "GTF") {
        genes <- .makeGenesFromGencodeGTF(genes)
    } else if (source == "RefSeq" && type == "GFF3") {
        genes <- .makeGenesFromRefSeqGFF3(genes)
    } else if (source == "WormBase" && type == "GTF") {
        genes <- .makeGenesFromWormBaseGTF(genes)
    } else {
        stop(paste(
            "Failed to make gene-level GRanges.",
            "Unsupported GFF source file.",
            sep = "\n"
        ))
    }

    names(genes) <- mcols(genes)[["gene_id"]]
    metadata(genes)[["level"]] <- "genes"

    if (level == "genes") {
        out <- genes
    }

    # Transcripts --------------------------------------------------------------
    if (level == "transcripts") {
        transcripts <- object
        transcripts <-
            transcripts[!is.na(mcols(transcripts)[["transcript_id"]])]
        assert(hasLength(transcripts))

        if (source == "Ensembl" && type == "GFF3") {
            transcripts <- .makeTranscriptsFromEnsemblGFF3(transcripts)
        } else if (source == "Ensembl" && type == "GTF") {
            transcripts <- .makeTranscriptsFromEnsemblGTF(transcripts)
        } else if (source == "FlyBase" && type == "GTF") {
            transcripts <- .makeTranscriptsFromFlyBaseGTF(transcripts)
        } else if (source == "GENCODE" && type == "GFF3") {
            transcripts <- .makeTranscriptsFromGencodeGFF3(transcripts)
        } else if (source == "GENCODE" && type == "GTF") {
            transcripts <- .makeTranscriptsFromGencodeGTF(transcripts)
        } else if (source == "RefSeq" && type == "GFF3") {
            transcripts <- .makeTranscriptsFromRefSeqGFF3(transcripts)
        } else if (source == "WormBase" && type == "GTF") {
            transcripts <- .makeTranscriptsFromWormBaseGTF(transcripts)
        } else {
            stop(paste(
                "Failed to make transcript-level GRanges.",
                "Unsupported GFF file format.",
                sep = "\n"
            ))
        }

        names(transcripts) <- mcols(transcripts)[["transcript_id"]]
        metadata(transcripts)[["level"]] <- "transcripts"

        if (source == "RefSeq") {
            message("Skipping gene metadata merge for RefSeq transcripts.")
            # Skip gene-level metadata merge for RefSeq transcripts.
            # This feature isn't supported yet, because RefSeq doesn't return
            # ranges 1:1 nicely for genes and transcripts.
            out <- transcripts
        } else {
            # By default, merge the gene-level annotations into the
            # transcript-level ones, for objects that have ranges 1:1 with the
            # identifiers.
            out <- .mergeGenesIntoTranscripts(
                transcripts = transcripts,
                genes = genes
            )
        }
    }

    # Return -------------------------------------------------------------------
    out <- .makeGRanges(out)
    assert(isSubset(c("detect", "level"), names(metadata(out))))

    # Ensure that the ranges match GenomicFeatures output, if desired.
    # Note that this step will error out for WormBase currently. Need to think
    # of a reworked approach that avoids this.
    if (is(out, "GRanges") && is(txdb, "TxDb")) {
        .checkGRangesAgainstTxDb(gr = out, txdb = txdb)
    }

    out
}



# GFF utils ====================================================================
# Report the source of the gene annotations.
.detectGFFSource <- function(object) {
    assert(is(object, "GRanges"))
    mcols <- mcols(object)
    source <- mcols[["source"]]
    if (
        # UCSC (e.g. hg38_knownGene)
        any(grepl(pattern = "_knownGene$", x = source, ignore.case = FALSE))
    ) {
        stop(paste(
            "UCSC is intentionally not supported.",
            "Use a pre-built TxDb package instead.",
            "(e.g. TxDb.Hsapiens.UCSC.hg38.knownGene)",
            sep = "\n"
        ))
    } else if (
        # Check for GENCODE prior to Ensembl.
        any(source == "ENSEMBL") &&
        any(source == "HAVANA") &&
        "gene_type" %in% colnames(mcols)
    ) {
        "GENCODE"
    } else if (
        any(grepl(pattern = "FlyBase", x = source, ignore.case = FALSE))
    ) {
        "FlyBase"
    } else if (
        any(grepl(pattern = "WormBase", x = source, ignore.case = FALSE))
    ) {
        "WormBase"
    } else if (
        any(grepl(pattern = "RefSeq", x = source, ignore.case = FALSE))
    ) {
        message(paste(
            "RefSeq support is experimental.",
            "Bioconductor has tighter integration with Ensembl.",
            sep = "\n"
        ))
        "RefSeq"
    } else if (
        any(grepl(
            pattern = "ensembl|havana",
            x = source,
            ignore.case = FALSE
        ))
    ) {
        "Ensembl"
    } else {
        stop(paste0(
            "Failed to detect valid GFF/GTF source.\n",
            "Supported: ",
            toString(c(
                "Ensembl",
                "GENCODE",
                "RefSeq",
                "FlyBase",
                "WormBase"
            ))
        ))
    }
}



# Determine if GFF or GTF.
.detectGFFType <- function(object) {
    assert(is(object, "GRanges"))
    if (any(c("ID", "Name", "Parent") %in% colnames(mcols(object)))) {
        "GFF3"
    } else {
        "GTF"
    }
}



# Remove uninformative metadata columns from GFF3 before return.
.minimizeGFF3 <- function(object) {
    assert(is(object, "GRanges"))
    mcols <- mcols(object)
    mcolnames <- colnames(mcols)
    blacklist <- c(
        "Alias",
        "ID",
        "Name",
        "Parent",
        "biotype"
    )
    mcolnames <- setdiff(mcolnames, blacklist)
    mcols <- mcols[, mcolnames, drop = FALSE]
    mcols(object) <- mcols
    object
}



.slotGFFDetectInfo <- function(object) {
    source <- .detectGFFSource(object)
    type <- .detectGFFType(object)
    message(paste(source, type, "detected."))
    metadata(object)[["detect"]] <- c(
        source = source,
        type = type
    )
    object
}



# TxDb comparison ==============================================================
# Check GRanges from GFF loaded with rtracklayer against GenomicFeatures TxDb.
# Consider adding support for exons, CDS, in a future update.
.checkGRangesAgainstTxDb <- function(gr, txdb) {
    assert(is(gr, "GRanges"), is(txdb, "TxDb"))
    level <- match.arg(
        arg = metadata(gr)[["level"]],
        choices = c("genes", "transcripts")
    )
    fun <- get(x = level, envir = asNamespace("basejump"), inherits = TRUE)
    assert(is.function(fun))
    message(paste("Checking", level, "in TxDb."))
    gr1 <- gr
    rm(gr)

    # Convert the TxDb to GRanges using either `genes()` or `transcripts()`.
    # Note that GenomicFeatures currently returns with "tx" instead of
    # "transcript" for transcript-level annotations.
    gr2 <- fun(txdb)
    assert(is(gr2, "GRanges"))

    # Ensure both ranges are camel case formatted.
    gr1 <- camel(gr1)
    gr2 <- camel(gr2)

    # Always set the names on GRanges from GFF.
    idCol <- .detectGRangesIDs(gr1)
    names(gr1) <- mcols(gr1)[[idCol]]

    if (
        level == "genes" &&
        hasLength(intersect(
            x = mcols(gr1)[["geneName"]],
            y = mcols(gr2)[["geneID"]]
        ))
    ) {
        # GenomicFeatures currently returns GFF3 input with gene symbols as the
        # names, so ensure we're setting the GRanges from GFF to match.
        message(paste(
            "TxDb returns gene names as identifiers for GFF3.",
            "Setting names on GRanges from GFF to match.",
            sep = "\n"
        ))
        names(gr1) <- mcols(gr1)[["geneName"]]
    } else if (
        level == "transcripts" &&
        hasLength(intersect(
            x = names(gr1),
            y = mcols(gr2)[["txName"]]
        ))
    ) {
        # `GenomicFeatures::transcripts()` returns numbers instead of correct
        # transcript IDs, so fix that before checks. Note that this return maps
        # the correct identifiers to `txName` column in `mcols()`.
        names(gr2) <- mcols(gr2)[["txName"]]
    } else if (
        level == "transcripts" &&
        !hasLength(intersect(
            x = names(gr1),
            y = mcols(gr2)[["txName"]]
        )) &&
        hasLength(intersect(
            x = mcols(gr1)[["transcriptName"]],
            y = mcols(gr2)[["txName"]]
        ))
    ) {
        # GenomicFeatures currently returns GFF3 input with transcript names
        # only, so ensure we're setting both GRanges accordingly.
        message(paste(
            "TxDb returns gene names as identifiers for GFF3.",
            "Setting names on GRanges from GFF to match.",
            sep = "\n"
        ))
        names(gr1) <- mcols(gr1)[["transcriptName"]]
        names(gr2) <- mcols(gr2)[["txName"]]
    }

    # Ensure both GRanges are sorted by names.
    assert(hasNames(gr1), hasNames(gr2))
    gr1 <- gr1[sort(names(gr1))]
    gr2 <- gr2[sort(names(gr2))]

    # Length and names ---------------------------------------------------------
    # Warn if GenomicFeatures has dropped ranges from GFF.
    # This can happen for some GFF3 files and FlyBase GTF.
    if (length(gr1) > length(gr2)) {
        assert(isSubset(names(gr2), names(gr1)))
        n <- length(gr1) - length(gr2)
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
            " from file to make TxDb.\n",
            "Missing in TxDb: ",
            toString(c(head(setdiff(names(gr1), names(gr2))), "..."))
        ))
        # Subset the GRanges from GFF to match TxDb for additional checks.
        gr1 <- gr1[names(gr2)]
    } else if (length(gr2) > length(gr1)) {
        # This scenario happens for GENCODE GTF, where we've dropped PAR Y dupes
        # from the GFF file but they still remain in GenomicFeatures.
        assert(isSubset(names(gr1), names(gr2)))
        n <- length(gr2) - length(gr1)
        warning(paste0(
            "basejump dropped ",
            sprintf(
                fmt = ngettext(
                    n = n,
                    msg1 = "%s identifier",
                    msg2 = "%s identifiers"
                ),
                n
            ),
            " from file to make GRanges.\n",
            "Missing in GRanges: ",
            toString(c(head(setdiff(names(gr2), names(gr1))), "..."))
        ))
        # Subset the GRanges from TxDb to match GFF for additional checks.
        gr2 <- gr2[names(gr1)]
    }
    assert(
        identical(length(gr1), length(gr2)),
        identical(names(gr1), names(gr2))
    )

    # Ranges and seqnames ------------------------------------------------------
    # Compare the ranges and inform the user about mismatches.
    # nolint start
    # > help(topic = "IPosRanges-comparison", package = "IRanges")
    # > vignette(topic = "IRangesOverview", package = "IRanges")
    # nolint end
    r1 <- ranges(gr1)
    r2 <- ranges(gr2)
    diff <- r1 != r2
    if (any(diff)) {
        which <- head(which(diff), n = 10L)
        warning(paste0(
            sum(diff, na.rm = TRUE),
            " range mismatches detected in TxDb.", "\n",
            "Showing GRanges mismatch comparison (first 10).", "\n\n",
            "(1) basejump:", "\n",
            printString(r1[which]), "\n\n",
            "(2) GenomicFeatures:", "\n",
            printString(r2[which]), "\n\n",
            "If the ranges in (1) are incorrect, please file an issue:", "\n",
            "  https://github.com/steinbaugh/basejump/issues", "\n",
            "If the ranges in (2) are incorrect, please file an issue:", "\n",
            "  https://github.com/Bioconductor/GenomicFeatures/issues"
        ))
    }
    if (!identical(seqnames(gr1), seqnames(gr2))) {
        warning("seqnames() mismatch detected.")
    }

    invisible(TRUE)
}



# Make TxDb from GRanges.
#
# Note that this uses different functions depending on whether the GRanges was
# created from a GFF3 or GTF file.
#
# We're now using the TxDb object for GRanges strict mode sanity checks. TxDb
# doesn't return enough useful metadata from the original GFF/GTF file, so we're
# parsing the return manually here using custom code instead. Note that this
# step must be called before we attempt to sanitize any metadata columns in
# `mcols()`.
#
# Note that Ensembl GFF3 currently fails this check.
# File an issue to request a fix for this in GenomicFeatures.
# - `makeTxDbFromGRanges()`:
#   some exons are linked to transcripts not found in the file
# - `makeTxDbFromGFF()` works but warns:
#   The following orphan exon were dropped
#   The following orphan CDS were dropped
.makeTxDbFromGFF <- function(object) {
    assert(
        is(object, "GRanges"),
        isSubset("detect", names(metadata(object)))
    )

    # Get stashed metadata values.
    file <- metadata(object)[["file"]]
    type <- metadata(object)[["detect"]][["type"]]

    txdb <- withCallingHandlers(expr = {
        # Using `tryCatch()` here to change error message, if necessary.
        tryCatch(
            expr = if (type == "GFF") {
                # `makeTxDbFromGRanges()` often chokes on GRanges from GFF3,
                # imported via `rtracklayer::import()`, so switch to using
                # `makeTxDbFromGFF()` instead, which always works.
                message("Making TxDb using makeTxDbFromGFF().")
                makeTxDbFromGFF(file)
            } else if (type == "GTF") {
                message("Making TxDb using makeTxDbFromGRanges().")
                makeTxDbFromGRanges(object)
            },
            error = function(e) {
                stop(paste0(
                    "Failed to make TxDb using ",
                    "GenomicFeatures::makeTxDbFromGRanges().\n",
                    "Set `strict = FALSE` to disable TxDb checks.\n",
                    conditionMessage(e)
                ))
            }
        )
    }, warning = function(w) {
        # Note that `makeTxDbFromGRanges()` frequently returns warnings.
        # Specifically, we're suppressing this expected warning:
        #   The "phase" metadata column contains non-NA values for
        #   features of type stop_codon. This information was ignored.
        # See also:
        # - https://stackoverflow.com/questions/38603668
        # - https://stackoverflow.com/questions/16517795
        if (grepl(pattern = "stop_codon", x = conditionMessage(w))) {
            invokeRestart("muffleWarning")
        }
    })

    assert(is(txdb, "TxDb"))
    txdb
}



# Aliases ======================================================================
#' @describeIn makeGRangesFromGFF GTF file extension alias.
#'   Runs the same internal code as [makeGRangesFromGFF()].
#' @export
makeGRangesFromGTF <- makeGRangesFromGFF
