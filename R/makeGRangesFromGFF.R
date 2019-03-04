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
    object <- import(file)
    # Slot the file path into metadata. Note that this may be a useful addition
    # to `brio::import()` in a future update.
    metadata(object)[["file"]] <- file
    metadata(object)[["level"]] <- level
    # Slot the source (e.g. Ensembl) and type (e.g. GTF) into `metadata()`.
    object <- .slotGFFDetectInfo(object)
    assert(is(object, "GRanges"))

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
    if (isTRUE(strict)) {
        message("Strict mode enabled. Checking against TxDb.")
        txdb <- .makeTxDbFromGFF(object)
    } else {
        txdb <- NULL
    }

    # Sanitization/standardization ---------------------------------------------
    if (source == "FlyBase") {
        object <- .sanitizeFlyBaseGFF(object)
    } else if (source == "RefSeq") {
        object <- .sanitizeRefSeqGFF(object)
    }

    # Pre-flight checks --------------------------------------------------------
    # Always require `gene_id` and `transcript_id` columns in file.
    assert(isSubset(
        x = c("gene_id", "transcript_id"),
        y = colnames(mcols(object))
    ))

    # Genes --------------------------------------------------------------------
    # This function always returns gene-level metadata, even when transcripts
    # are requested. We'll merge this into the transcript-level GRanges below,
    # if necessary.
    genes <- .makeGenesFromGFF(object = object, txdb = txdb)
    if (level == "genes") {
        out <- genes
    }

    # Transcripts --------------------------------------------------------------
    if (level == "transcripts") {
        transcripts <- .makeTranscriptsFromGFF(object = object, txdb = txdb)
        if (is(transcripts, "GRangesList")) {
            # Skip gene-level metadata merge for RefSeq transcripts.
            # This feature isn't supported yet, because RefSeq doesn't return
            # ranges 1:1 nicely for genes and transcripts.
            out <- transcripts
        } else {
            # Merge the gene-level annotations into the transcript-level ones.
            out <- .mergeGenesIntoTranscripts(
                transcripts = transcripts,
                genes = genes
            )
        }
    }

    # Metadata -----------------------------------------------------------------
    assert(isSubset(
        x = c("detect", "file", "level"),
        y = names(metadata(out))
    ))

    # Return with same formatting conventions for `makeGRangesFromEnsembl()`.
    # This returns with `mcols()` renamed in camel case. Empty columns in
    # `mcols()` will get dropped automatically.
    .makeGRanges(out)
}



# Sanitizers per source ========================================================
.sanitizeFlyBaseGFF <- function(object) {
    assert(is(object, "GRanges"))
    # Rename `gene_symbol`, `transcript_symbol` to use `*_name` instead,
    # matching Ensembl GTF spec.
    colnames(mcols(object)) <- gsub(
        pattern = "_symbol$",
        replacement = "_name",
        x = colnames(mcols(object))
    )
    object
}



.sanitizeRefSeqGFF <- function(object) {
    assert(is(object, "GRanges"))
    # Rename `gene` column to `gene_id`, matching Ensembl spec.
    colnames(mcols(object)) <- sub(
        pattern = "^gene$",
        replacement = "gene_id",
        x = colnames(mcols(object))
    )
    object
}



# Per file parsing functions ===================================================
# This is the main constructor function for gene-level GRanges from GFF/GTF.
# Including txdb passthrough here for strict mode checks.
.makeGenesFromGFF <- function(object, txdb = NULL) {
    assert(
        is(object, "GRanges"),
        isAny(txdb, c("TxDb", "NULL")),
        isSubset("detect", names(metadata(object)))
    )
    type <- metadata(object)[["detect"]][["type"]]
    source <- metadata(object)[["detect"]][["source"]]

    # Drop any rows that aren't gene specific.
    object <- object[!is.na(mcols(object)[["gene_id"]])]
    object <- object[is.na(mcols(object)[["transcript_id"]])]
    assert(hasLength(object))

    if (type == "GTF") {
        object <- .makeGenesFromGTF(object)
    } else if (source == "Ensembl" && type == "GFF") {
        object <- .makeGenesFromEnsemblGFF(object)
    } else if (source == "RefSeq" && type == "GFF") {
        object <- .makeGenesFromRefSeqGFF(object)
    } else {
        stop("Unsupported file.")
    }

    # Ensure the ranges are sorted by gene identifier.
    if (is(object, "GRanges")) {
        assert(
            hasLength(object),
            !any(is.na(mcols(object)[["gene_id"]])),
            hasNoDuplicates(mcols(object)[["gene_id"]])
        )
        names(object) <- mcols(object)[["gene_id"]]
    }
    object <- object[sort(names(object))]

    # Ensure that the ranges match GenomicFeatures output.
    if (is(object, "GRanges") && is(txdb, "TxDb")) {
        .checkGRangesAgainstTxDb(gr = object, txdb = txdb, level = "genes")
    }

    # WormBase identifier fix. WormBase GTF currently imports somewhat
    # malformed, and the gene identifiers require additional sanitization to
    # return correctly. Some garbage rows containing "Gene:" or "Transcript:"
    # will remain. We need to drop these before proceeding. Note that this
    # step needs to be called after TxDb strict mode check.
    if (source == "WormBase" && type == "GTF") {
        keep <- !grepl(pattern = ":", x = mcols(object)[["gene_id"]])
        object <- object[keep]
    }

    message(paste(length(object), "gene annotations detected."))
    object
}



# Note that before we return gene-level ranges from WormBase, we need to
# drop some additional malformed rows. Refer to `.makeGenesFromGFF()`.
.makeGenesFromGTF <- function(object) {
    assert(is(object, "GRanges"))
    object <- object[mcols(object)[["type"]] == "gene"]
    object
}



.makeGenesFromEnsemblGFF <- function(object) {
    # Drop rows that contain a parent element.
    keep <- vapply(
        X = mcols(object)[["Parent"]],
        FUN = function(x) {
            identical(x, character(0))
        },
        FUN.VALUE = logical(1L)
    )
    object <- object[keep]

    # Note that "gene" type alone doesn't return all expected identifiers
    # for Ensembl GFF3, but it works for Ensembl GTF.
    keep <- grepl(
        pattern = paste(
            c(
                "^gene$",
                "^pseudogene$",
                "_gene$",
                "^bidirectional_promoter_lncRNA$"  # e.g. ENSG00000176236
            ),
            collapse = "|"
        ),
        x = mcols(object)[["type"]],
        ignore.case = TRUE
    )
    object <- object[keep]

    # Assign `gene_name` column using `Name` column.
    assert(
        isSubset("Name", colnames(mcols(object))),
        areDisjointSets("gene_name", colnames(mcols(object)))
    )
    mcols(object)[["gene_name"]] <- mcols(object)[["Name"]]
    mcols(object)[["Name"]] <- NULL

    # Assign `gene_biotype` column.
    assert(
        isSubset("biotype", colnames(mcols(object))),
        areDisjointSets("gene_biotype", colnames(mcols(object)))
    )
    mcols(object)[["gene_biotype"]] <- mcols(object)[["biotype"]]
    mcols(object)[["biotype"]] <- NULL

    # Remove extra columns.
    mcols(object)[["Alias"]] <- NULL
    mcols(object)[["ID"]] <- NULL
    mcols(object)[["Parent"]] <- NULL

    object
}



.makeGenesFromRefSeqGFF <- function(object) {
    object <- .sanitizeRefSeqGFF(object)

    # Drop rows that contain a parent element.
    keep <- vapply(
        X = mcols(object)[["Parent"]],
        FUN = function(x) {
            identical(x, character(0))
        },
        FUN.VALUE = logical(1L)
    )
    object <- object[keep]

    # Only keep genes and pseudogenes.
    # Note that FlyBase uses non-standard transcript types.
    keep <- grepl(
        pattern = paste(c("^gene$", "^pseudogene$"), collapse = "|"),
        x = mcols(object)[["type"]],
        ignore.case = TRUE
    )
    object <- object[keep]

    # Assign `gene_name` column using `Name` column.
    assert(
        isSubset("Name", colnames(mcols(object))),
        areDisjointSets("gene_name", colnames(mcols(object)))
    )
    mcols(object)[["gene_name"]] <- mcols(object)[["Name"]]
    mcols(object)[["Name"]] <- NULL

    # Check for `gene_biotype` column.
    assert(isSubset("gene_biotype", colnames(mcols(object))))

    # Remove extra columns.
    mcols(object)[["ID"]] <- NULL
    mcols(object)[["Parent"]] <- NULL

    # RefSeq contains multiple ranges per gene, so `split()` into a
    # `GRangesList` object:
    # [1] "pseudogene"           "gene"
    # [3] "enhancer"             "promoter"
    # [5] "sequence_feature"     "recombination_region"

    if (any(duplicated(mcols(object)[["gene_id"]]))) {
        message(paste(
            "RefSeq contains multiple ranges per gene.",
            "Splitting into GRangesList.",
            sep = "\n"
        ))
        object <- split(x = object, f = mcols(object)[["gene_id"]])
    }

    # FIXME Need to early return this as GRangesList.
    # Need to call `.makeGRanges()` before `split()` step.
    object
}



# This is the main transcript-level GRanges generator.
.makeTranscriptsFromGFF <- function(object, txdb = NULL) {
    assert(
        is(object, "GRanges"),
        isAny(txdb, c("TxDb", "NULL")),
        isSubset("detect", names(metadata(object)))
    )
    type <- metadata(object)[["detect"]][["type"]]
    source <- metadata(object)[["detect"]][["source"]]

    object <- object[!is.na(mcols(object)[["transcript_id"]])]
    assert(hasLength(object))

    if (source != "FlyBase" && type == "GTF") {
        # Subset using the `type` column.
        # Note that this method doesn't work for FlyBase (see below).
        object <- object[mcols(object)[["type"]] == "transcript"]
    } else if (source == "FlyBase" && type == "GTF") {
        object <- .makeTranscriptsFromFlyBaseGTF(object)
    } else if (source == "Ensembl" && type == "GFF") {
        object <- .makeTranscriptsFromEnsemblGFF(object)
    } else if (source == "RefSeq" && type == "GFF") {
        object <- .makeTranscriptsFromRefSeqGFF(object)
    } else {
        stop("Unsupported file.")
    }

    # Ensure the ranges are sorted by transcript identifier.
    assert(
        hasLength(object),
        !any(is.na(mcols(object)[["transcript_id"]])),
        hasNoDuplicates(mcols(object)[["transcript_id"]])
    )
    names(object) <- mcols(object)[["transcript_id"]]
    object <- object[sort(names(object))]

    # Ensure that the ranges match GenomicFeatures output.
    if (is(object, "GRanges") && is(txdb, "TxDb")) {
        .checkGRangesAgainstTxDb(
            gr = object,
            txdb = txdb,
            level = "transcripts"
        )
    }

    message(paste(length(object), "transcript annotations detected."))
    object
}



.makeTranscriptsFromEnsemblGFF <- function(object) {
    assert(is(object, "GRanges"))

    # Assign `transcript_name` from `Name` column.
    assert(
        isSubset("Name", colnames(mcols(object))),
        areDisjointSets("transcript_name", colnames(mcols(object)))
    )
    mcols(object)[["transcript_name"]] <- mcols(object)[["Name"]]
    mcols(object)[["Name"]] <- NULL

    # Assign `transcript_biotype`.
    assert(
        isSubset("biotype", colnames(mcols(object))),
        areDisjointSets("transcript_biotype", colnames(mcols(object)))
    )
    mcols(object)[["transcript_biotype"]] <- mcols(object)[["biotype"]]
    mcols(object)[["biotype"]] <- NULL

    # Assign `gene_id` from `Parent`.
    assert(
        isSubset("Parent", colnames(mcols(object))),
        all(grepl("^gene:", mcols(object)[["Parent"]]))
    )
    mcols(object)[["gene_id"]] <- as.character(mcols(object)[["Parent"]])
    mcols(object)[["gene_id"]] <- gsub(
        pattern = "^gene:",
        replacement = "",
        x = mcols(object)[["gene_id"]]
    )

    # Remove extra columns.
    mcols(object)[["Alias"]] <- NULL
    mcols(object)[["ID"]] <- NULL
    mcols(object)[["Parent"]] <- NULL

    object
}



.makeTranscriptsFromFlyBaseGTF <- function(object) {
    object <- .sanitizeFlyBaseGFF(object)

    # Note that FlyBase uses non-standard transcript types.
    keep <- grepl(
        pattern = paste(c("^pseudogene$", "RNA$"), collapse = "|"),
        x = mcols(object)[["type"]],
        ignore.case = TRUE
    )
    object <- object[keep]

    object
}



.makeTranscriptsFromRefSeqGFF <- function(object) {
    object <- .sanitizeRefSeq(object)

    # Assign `transcript_name` from `Name` column.
    assert(
        isSubset("Name", colnames(mcols(object))),
        areDisjointSets("transcript_name", colnames(mcols(object)))
    )
    mcols(object)[["transcript_name"]] <- mcols(object)[["Name"]]
    mcols(object)[["Name"]] <- NULL

    # Remove extra columns.
    mcols(object)[["ID"]] <- NULL
    mcols(object)[["Parent"]] <- NULL

    # Here are the types that map to `transcript_id`:
    #  [1] "antisense_RNA"      "exon"
    #  [3] "guide_RNA"          "lnc_RNA"
    #  [5] "mRNA"               "primary_transcript"
    #  [7] "RNase_MRP_RNA"      "RNase_P_RNA"
    #  [9] "rRNA"               "scRNA"
    # [11] "snoRNA"             "snRNA"
    # [13] "telomerase_RNA"     "transcript"
    # [15] "vault_RNA"          "Y_RNA"

    if (any(duplicated(mcols(object)[["transcript_id"]]))) {
        message(paste(
            "RefSeq contains multiple ranges per transcript.",
            "Splitting into GRangesList.",
            sep = "\n"
        ))
        object <- split(x = object, f = mcols(object)[["transcript_id"]])
    }

    # FIXME Need to early return this as GRangesList.
    # Need to call `.makeGRanges()` before `split()` step.
    object
}



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
    # FIXME Store the level in GRanges metadata, so we don't have to pass
    # through the formal here.
    level <- match.arg(level)
    fun <- get(level)
    assert(is.function(fun))
    message(paste("Checking", level, "in TxDb."))

    # Convert the TxDb to GRanges using either `genes()` or `transcripts()`.
    # Note that GenomicFeatures currently returns with "tx_" instead of
    # "transcript_" for transcript-level annotations.
    grTxDb <- fun(txdb)
    assert(is(grTxDb, "GRanges"))

    if (
        level == "genes" &&
        hasLength(intersect(
            x = mcols(gr)[["gene_name"]],
            y = mcols(grTxDb)[["gene_id"]]
        ))
    ) {
        # GenomicFeatures currently returns GFF3 input with gene symbols as the
        # names, so ensure we're setting the GRanges from GFF to match.
        message(paste(
            "TxDb returns gene names as identifiers for GFF3.",
            "Setting names on GRanges from GFF to match.",
            sep = "\n"
        ))
        names(gr) <- mcols(gr)[["gene_name"]]
    } else if (
        level == "transcripts" &&
        hasLength(intersect(
            x = names(gr),
            y = mcols(grTxDb)[["tx_name"]]
        ))
    ) {
        # `GenomicFeatures::transcripts()` returns numbers instead of correct
        # transcript IDs, so fix that before checks. Note that this return maps
        # the correct identifiers to `tx_name` column in `mcols()`.
        names(grTxDb) <- mcols(grTxDb)[["tx_name"]]
    } else if (
        level == "transcripts" &&
        !hasLength(intersect(
            x = names(gr),
            y = mcols(grTxDb)[["tx_name"]]
        )) &&
        hasLength(intersect(
            x = mcols(gr)[["transcript_name"]],
            y = mcols(grTxDb)[["tx_name"]]
        ))
    ) {
        # GenomicFeatures currently returns GFF3 input with transcript names
        # only, so ensure we're setting both GRanges accordingly.
        message(paste(
            "TxDb returns gene names as identifiers for GFF3.",
            "Setting names on GRanges from GFF to match.",
            sep = "\n"
        ))
        names(gr) <- mcols(gr)[["transcript_name"]]
        names(grTxDb) <- mcols(grTxDb)[["tx_name"]]
    }
    assert(hasNames(grTxDb))

    # Ensure both GRanges are sorted by names.
    gr <- gr[sort(names(gr))]
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
            " from file to make TxDb.\n",
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
        isSubset(
            x = c("detect", "file"),
            y = names(metadata(object))
        )
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
    txdb
}



# Merge the gene-level annotations (`geneName`, `geneBiotype`) into a
# transcript-level GRanges object.
.mergeGenesIntoTranscripts <- function(transcripts, genes) {
    message("Merging gene-level annotations into transcript-level object.")
    assert(
        is(transcripts, "GRanges"),
        is(genes, "GRanges")
    )
    geneCols <- setdiff(
        x = colnames(mcols(genes)),
        y = colnames(mcols(transcripts))
    )
    if (length(geneCols) > 0L) {
        geneCols <- c("gene_id", geneCols)
        merge <- merge(
            x = mcols(transcripts),
            y = mcols(genes)[, geneCols, drop = FALSE],
            all.x = TRUE,
            by = "gene_id"
        )
        rownames(merge) <- merge[["transcript_id"]]
        merge <- merge[sort(rownames(merge)), , drop = FALSE]
        assert(identical(
            x = mcols(transcripts)[["transcript_id"]],
            y = merge[["transcript_id"]]
        ))
        mcols(transcripts) <- merge
    }
    transcripts
}



# GFF source info ==============================================================
# Report the source of the gene annotations.
.detectGFFSource <- function(object) {
    assert(is(object, "GRanges"))
    if (
        any(grepl(
            pattern = "FlyBase",
            x = mcols(object)[["source"]],
            ignore.case = FALSE
        ))
    ) {
        "FlyBase"
    } else if (
        any(grepl(
            pattern = "WormBase",
            x = mcols(object)[["source"]],
            ignore.case = FALSE
        ))
    ) {
        "WormBase"
    } else if (
        any(grepl(
            pattern = "RefSeq",
            x = mcols(object)[["source"]],
            ignore.case = FALSE
        ))
    ) {
        "RefSeq"
    } else if (
        # e.g. hg38_knownGene
        any(grepl(
            pattern = "_knownGene$",
            x = mcols(object)[["source"]],
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
            x = mcols(object)[["source"]],
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



# Determine if GFF or GTF.
# May be able to improve this step by checking against more columns.

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

# RefSeq GFF
#  [1] "source"                    "type"
#  [3] "score"                     "phase"
#  [5] "ID"                        "Dbxref"
#  [7] "Name"                      "chromosome"
#  [9] "gbkey"                     "genome"
# [11] "mol_type"                  "description"
# [13] "gene"                      "gene_biotype"
# [15] "pseudo"                    "Parent"
# [17] "product"                   "transcript_id"
# [19] "gene_synonym"              "model_evidence"
# [21] "protein_id"                "Note"
# [23] "exception"                 "inference"
# [25] "standard_name"             "experiment"
# [27] "function"                  "regulatory_class"
# [29] "feat_class"                "recombination_class"
# [31] "rpt_type"                  "rpt_unit_seq"
# [33] "anticodon"                 "partial"
# [35] "start_range"               "end_range"
# [37] "transl_except"             "mobile_element_type"
# [39] "rpt_family"                "satellite"
# [41] "bound_moiety"              "Target"
# [43] "assembly_bases_aln"        "assembly_bases_seq"
# [45] "bit_score"                 "blast_aligner"
# [47] "blast_score"               "common_component"
# [49] "e_value"                   "filter_score"
# [51] "for_remapping"             "gap_count"
# [53] "hsp_percent_coverage"      "matchable_bases"
# [55] "matched_bases"             "num_ident"
# [57] "num_mismatch"              "pct_coverage"
# [59] "pct_coverage_hiqual"       "pct_identity_gap"
# [61] "pct_identity_gapopen_only" "pct_identity_ungap"
# [63] "rank"                      "weighted_identity"
# [65] "lxr_locAcc_currStat_120"   "not_for_annotation"
# [67] "consensus_splices"         "exon_identity"
# [69] "identity"                  "idty"
# [71] "matches"                   "product_coverage"
# [73] "splices"                   "Gap"
# [75] "merge_aligner"             "map"
# [77] "part"                      "lxr_locAcc_currStat_35"
# [79] "direction"                 "rpt_unit_range"
# [81] "exon_number"               "number"
# [83] "allele"                    "align_id"
# [85] "batch_id"                  "crc32"
# [87] "curated_alignment"         "promoted_rank"
# [89] "qtaxid"                    "Is_circular"
# [91] "country"                   "isolation-source"
# [93] "note"                      "tissue-type"
# [95] "codons"                    "transl_table"

.detectGFFType <- function(object) {
    assert(is(object, "GRanges"))
    if (all(c("ID", "Name") %in% colnames(mcols(object)))) {
        "GFF"
    } else {
        "GTF"
    }
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



# Aliases ======================================================================
#' @describeIn makeGRangesFromGFF GTF file extension alias.
#'   Runs the same internal code as [makeGRangesFromGFF()].
#' @export
makeGRangesFromGTF <- makeGRangesFromGFF
