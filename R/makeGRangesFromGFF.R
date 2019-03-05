# Note that case sensitivity in `mcols()` is useful for initial checks, so
# don't immediately apply `camel()` after `import()` call.



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
    metadata(object)[["file"]] <- file
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

    # Standardize/sanitize -----------------------------------------------------
    # Run this step after TxDb generation. Ensembl, GENCODE, and WormBase files
    # follow expected (Ensembl-like) naming conventions.
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
        areDisjointSets("gene_type", mcolnames)
    )
    rm(mcolnames)

    # Genes --------------------------------------------------------------------
    # `makeGRangesFromGFF()` attempts to always returns gene-level metadata,
    # even when transcripts are requested. We'll merge this object into the
    # transcript-level GRanges below, if necessary.
    genes <- .makeGenesFromGFF(object)
    if (level == "genes") {
        out <- genes
    }

    # Transcripts --------------------------------------------------------------
    if (level == "transcripts") {
        transcripts <- .makeTranscriptsFromGFF(object)
        if (source == "RefSeq") {
            message(
                "Skipping gene-level metadata merge for RefSeq transcripts."
            )
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

    assert(isSubset(
        x = c("detect", "file", "level"),
        y = names(metadata(out))
    ))

    # Ensure that the ranges match GenomicFeatures output, if desired.
    # Note that this step will error out for WormBase currently. Need to think
    # of a reworked approach that avoids this.
    if (is(out, "GRanges") && is(txdb, "TxDb")) {
        .checkGRangesAgainstTxDb(gr = out, txdb = txdb)
    }

    out
}



# GFF source info ==============================================================
# Report the source of the gene annotations.
.detectGFFSource <- function(object) {
    assert(is(object, "GRanges"))
    mcols <- mcols(object)
    source <- mcols[["source"]]
    if (
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
        warning(paste(
            "RefSeq support is experimental.",
            "Bioconductor has tighter integration and support for Ensembl.",
            sep = "\n"
        ))
        "RefSeq"
    } else if (
        # e.g. hg38_knownGene
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
        any(grepl(
            pattern = "ensembl|havana",
            x = source,
            ignore.case = FALSE
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
.detectGFFType <- function(object) {
    assert(is(object, "GRanges"))
    if (any(c("ID", "Name", "Parent") %in% colnames(mcols(object)))) {
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



# Main gene and transcript parsers =============================================
# This is the main constructor function for gene-level GRanges from GFF/GTF.
# Including txdb passthrough here for strict mode checks.
.makeGenesFromGFF <- function(object) {
    assert(
        is(object, "GRanges"),
        isSubset("detect", names(metadata(object))),
        isSubset(c("gene_id", "transcript_id"), colnames(mcols(object)))
    )
    type <- metadata(object)[["detect"]][["type"]]
    source <- metadata(object)[["detect"]][["source"]]

    # Drop any rows that aren't gene specific.
    object <- object[!is.na(mcols(object)[["gene_id"]])]
    object <- object[is.na(mcols(object)[["transcript_id"]])]
    assert(hasLength(object))

    if (source == "Ensembl" && type == "GFF") {
        object <- .makeGenesFromEnsemblGFF3(object)
    } else if (source == "Ensembl" && type == "GTF") {
        object <- .makeGenesFromEnsemblGTF(object)
    } else if (source == "FlyBase" && type == "GTF") {
        object <- .makeGenesFromFlyBaseGTF(object)
    } else if (source == "GENCODE" && type == "GFF") {
        object <- .makeGenesFromGencodeGFF3(object)
    } else if (source == "GENCODE" && type == "GTF") {
        object <- .makeGenesFromGencodeGTF(object)
    } else if (source == "RefSeq" && type == "GFF") {
        object <- .makeGenesFromRefSeqGFF3(object)
    } else if (source == "WormBase" && type == "GTF") {
        object <- .makeGenesFromWormBaseGTF(object)
    } else {
        stop("Unsupported GFF file format.")
    }

    names(object) <- mcols(object)[["gene_id"]]
    metadata(object)[["level"]] <- "genes"

    object
}



# This is the main transcript-level GRanges generator.
.makeTranscriptsFromGFF <- function(object) {
    assert(
        is(object, "GRanges"),
        isSubset("detect", names(metadata(object))),
        isSubset("transcript_id", colnames(mcols(object)))
    )
    type <- metadata(object)[["detect"]][["type"]]
    source <- metadata(object)[["detect"]][["source"]]

    object <- object[!is.na(mcols(object)[["transcript_id"]])]
    assert(hasLength(object))

    if (source == "Ensembl" && type == "GFF") {
        object <- .makeTranscriptsFromEnsemblGFF3(object)
    } else if (source == "Ensembl" && type == "GTF") {
        object <- .makeTranscriptsFromEnsemblGTF(object)
    } else if (source == "FlyBase" && type == "GTF") {
        object <- .makeTranscriptsFromFlyBaseGTF(object)
    } else if (source == "GENCODE" && type == "GFF") {
        object <- .makeTranscriptsFromGencodeGFF3(object)
    } else if (source == "GENCODE" && type == "GTF") {
        object <- .makeTranscriptsFromGencodeGTF(object)
    } else if (source == "RefSeq" && type == "GFF") {
        object <- .makeTranscriptsFromRefSeqGFF3(object)
    } else if (source == "WormBase" && type == "GTF") {
        object <- .makeTranscriptsFromWormBaseGTF(object)
    } else {
        stop("Unsupported GFF file format.")
    }

    names(object) <- mcols(object)[["transcript_id"]]
    metadata(object)[["level"]] <- "transcripts"

    object
}



# FIXME Can we consolidate this with `makeGRangesFromEnsembl()`?
# FIXME This may mess up from the ensembldb output, which is camel?

# Merge the gene-level annotations (`geneName`, `geneBiotype`) into a
# transcript-level GRanges object.
.mergeGenesIntoTranscripts <- function(transcripts, genes) {
    message("Merging gene-level annotations into transcript-level object.")
    assert(
        is(transcripts, "GRanges"),
        is(genes, "GRanges"),
        hasNames(transcripts),
        isSubset("transcript_id", colnames(mcols(transcripts))),
        identical(names(transcripts), mcols(transcripts)[["transcript_id"]]),
        # Don't proceed unless we have `gene_id` column to use for merge.
        isSubset("gene_id", colnames(mcols(transcripts))),
        isSubset("gene_id", colnames(mcols(genes)))
    )
    geneCols <- setdiff(
        x = colnames(mcols(genes)),
        y = colnames(mcols(transcripts))
    )
    # Only attempt the merge if there's useful additional metadata to include.
    # Note that base `merge()` can reorder rows, so be careful here.
    if (length(geneCols) > 0L) {
        geneCols <- c("gene_id", geneCols)
        merge <- merge(
            x = mcols(transcripts),
            y = mcols(genes)[, geneCols, drop = FALSE],
            all.x = TRUE,
            by = "gene_id"
        )
        # Ensure that we're calling `S4Vectors::merge()`, not `base::merge()`.
        assert(is(merge, "DataFrame"))
        # The merge step will drop row names, so we need to reassign.
        rownames(merge) <- merge[["transcript_id"]]
        # Reorder to match the original transcripts object.
        # Don't assume this is alphabetically sorted.
        merge <- merge[names(transcripts), , drop = FALSE]
        assert(identical(
            x = mcols(transcripts)[["transcript_id"]],
            y = merge[["transcript_id"]]
        ))
        mcols(transcripts) <- merge
    }
    transcripts
}



# Ensembl GTF/GFF3 =============================================================
# GTF:
#>  [1] "source"                   "type"
#>  [3] "score"                    "phase"
#>  [5] "gene_id"                  "gene_version"
#>  [7] "gene_name"                "gene_source"
#>  [9] "gene_biotype"             "transcript_id"
#> [11] "transcript_version"       "transcript_name"
#> [13] "transcript_source"        "transcript_biotype"
#> [15] "tag"                      "transcript_support_level"
#> [17] "exon_number"              "exon_id"
#> [19] "exon_version"             "protein_id"
#> [21] "protein_version"          "ccds_id"
#
# GFF:
#>  [1] "source"                   "type"
#>  [3] "score"                    "phase"
#>  [5] "ID"                       "Alias"
#>  [7] "external_name"            "logic_name"
#>  [9] "Name"                     "biotype"
#> [11] "description"              "gene_id"
#> [13] "version"                  "Parent"
#> [15] "tag"                      "transcript_id"
#> [17] "transcript_support_level" "constitutive"
#> [19] "ensembl_end_phase"        "ensembl_phase"
#> [21] "exon_id"                  "rank"
#> [23] "protein_id"               "ccdsid"



# Selecting by "gene" type works consistently for GTF.
.makeGenesFromEnsemblGTF <- function(object) {
    assert(is(object, "GRanges"))
    object <- object[mcols(object)[["type"]] == "gene"]
    object
}



.makeGenesFromEnsemblGFF3 <- function(object) {
    # FIXME I think this step is unnecessary.
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
                # e.g. ENSG00000176236
                "^bidirectional_promoter_lncRNA$"
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



.makeTranscriptsFromEnsemblGTF <- function(object) {
    assert(is(object, "GRanges"))
    object <- object[mcols(object)[["type"]] == "transcript"]
    object
}



.makeTranscriptsFromEnsemblGFF3 <- function(object) {
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



# FlyBase GTF ==================================================================
# Match Ensembl spec by renaming `*_symbol` to `*_name`.
.standardizeFlyBaseToEnsembl <- function(object) {
    assert(is(object, "GRanges"))
    mcolnames <- colnames(mcols(object))
    mcolnames <- sub(
        pattern = "^gene_symbol$",
        replacement = "gene_name",
        x = mcolnames
    )
    mcolnames <- sub(
        pattern = "^transcript_symbol$",
        replacement = "transcript_name",
        x = mcolnames
    )
    colnames(mcols(object)) <- mcolnames
    object
}



# Compatible with Ensembl importer after we run `standardizeFlyBaseGFF()`.
.makeGenesFromFlyBaseGTF <- function(object) {
    object <- .standardizeFlyBaseToEnsembl(object)
    object <- .makeGenesFromEnsemblGTF(object)
    object
}



.makeTranscriptsFromFlyBaseGTF <- function(object) {
    object <- .standardizeFlyBaseToEnsembl(object)
    # Note that FlyBase uses non-standard transcript types.
    keep <- grepl(
        pattern = paste(c("^pseudogene$", "RNA$"), collapse = "|"),
        x = mcols(object)[["type"]],
        ignore.case = TRUE
    )
    object <- object[keep]
    object
}



# GENCODE GTF/GFF3 =============================================================
# GTF
#>  [1] "source"                   "type"
#>  [3] "score"                    "phase"
#>  [5] "gene_id"                  "gene_type"
#>  [7] "gene_name"                "level"
#>  [9] "havana_gene"              "transcript_id"
#> [11] "transcript_type"          "transcript_name"
#> [13] "transcript_support_level" "tag"
#> [15] "havana_transcript"        "exon_number"
#> [17] "exon_id"                  "ont"
#> [19] "protein_id"               "ccdsid"
#
# GFF
#>  [1] "source"                   "type"
#>  [3] "score"                    "phase"
#>  [5] "ID"                       "gene_id"
#>  [7] "gene_type"                "gene_name"
#>  [9] "level"                    "havana_gene"
#> [11] "Parent"                   "transcript_id"
#> [13] "transcript_type"          "transcript_name"
#> [15] "transcript_support_level" "tag"
#> [17] "havana_transcript"        "exon_number"
#> [19] "exon_id"                  "ont"
#> [21] "protein_id"               "ccdsid"
#
# GENCODE uses `gene_type` instead of `gene_biotype`.
# Note that `gene_id` and `gene_name` are nicely defined, so don't use `Name`.



.standardizeGencodeToEnsembl <- function(object) {
    assert(is(object, "GRanges"))
    mcolnames <- colnames(mcols(object))
    mcolnames <- sub(
        pattern = "^gene_type$",
        replacement = "gene_biotype",
        x = mcolnames
    )
    mcolnames <- sub(
        pattern = "^transcript_type$",
        replacement = "transcript_biotype",
        x = mcolnames
    )
    colnames(mcols(object)) <- mcolnames
    object
}



# Remove the duplicate PAR Y chromosome annotations, to match Ensembl spec.
.singlePAR <- function(object, idCol) {
    idCol <- match.arg(
        arg = idCol,
        choices = c("ID", "gene_id", "transcript_id")
    )
    parY <- grepl(pattern = "_PAR_Y$", x = mcols(object)[[idCol]])
    if (hasLength(parY)) {
        message(paste(
            "Removing", sum(parY, na.remove = TRUE),
            "pseudoautosomal region (PAR) Y chromosome duplicates."
        ))
        object <- object[!parY]
    }
    object
}



.makeGenesFromGencodeGTF <- function(object, singlePAR = TRUE) {
    object <- .makeGenesFromEnsemblGTF(object)
    # Remove duplicate PARs on Y chromosome.
    if (isTRUE(singlePAR)) {
        object <- .singlePAR(object, idCol = "gene_id")
    }
    object
}



.makeGenesFromGencodeGFF3 <- function(object, singlePAR = TRUE) {
    object <- .standardizeGencodeToEnsembl(object)
    assert(
        isSubset(
            x = c("gene_id", "gene_name", "gene_biotype"),
            y = colnames(mcols(object))
        ),
        isFlag(singlePAR)
    )

    # Only keep rows that match gene type.
    keep <- mcols(object)[["type"]] == "gene"
    object <- object[keep]

    # Remove duplicate PARs on Y chromosome.
    # Note that for GFF3, this is defined in the "ID" column, not "gene_id".
    if (isTRUE(singlePAR)) {
        object <- .singlePAR(object, idCol = "ID")
    }

    # Remove extra columns.
    mcols(object)[["Alias"]] <- NULL
    mcols(object)[["ID"]] <- NULL
    mcols(object)[["Parent"]] <- NULL

    object
}



.makeTranscriptsFromGencodeGTF <- function(object, singlePAR = TRUE) {
    object <- .makeTranscriptsFromEnsemblGTF(object)
    # Remove duplicate PARs on Y chromosome.
    if (isTRUE(singlePAR)) {
        object <- .singlePAR(object, idCol = "transcript_id")
    }
    object
}



# FIXME Inform the user about number of PAR genes detected.
.makeTranscriptsFromGencodeGFF3 <- function(object, singlePAR = TRUE) {
    # FIXME
    stop("NOT SUPPORTED YET.")
}



# RefSeq GFF3 ==================================================================
# GFF
#>  [1] "source"                    "type"
#>  [3] "score"                     "phase"
#>  [5] "ID"                        "Dbxref"
#>  [7] "Name"                      "chromosome"
#>  [9] "gbkey"                     "genome"
#> [11] "mol_type"                  "description"
#> [13] "gene"                      "gene_biotype"
#> [15] "pseudo"                    "Parent"
#> [17] "product"                   "transcript_id"
#> [19] "gene_synonym"              "model_evidence"
#> [21] "protein_id"                "Note"
#> [23] "exception"                 "inference"
#> [25] "standard_name"             "experiment"
#> [27] "function"                  "regulatory_class"
#> [29] "feat_class"                "recombination_class"
#> [31] "rpt_type"                  "rpt_unit_seq"
#> [33] "anticodon"                 "partial"
#> [35] "start_range"               "end_range"
#> [37] "transl_except"             "mobile_element_type"
#> [39] "rpt_family"                "satellite"
#> [41] "bound_moiety"              "Target"
#> [43] "assembly_bases_aln"        "assembly_bases_seq"
#> [45] "bit_score"                 "blast_aligner"
#> [47] "blast_score"               "common_component"
#> [49] "e_value"                   "filter_score"
#> [51] "for_remapping"             "gap_count"
#> [53] "hsp_percent_coverage"      "matchable_bases"
#> [55] "matched_bases"             "num_ident"
#> [57] "num_mismatch"              "pct_coverage"
#> [59] "pct_coverage_hiqual"       "pct_identity_gap"
#> [61] "pct_identity_gapopen_only" "pct_identity_ungap"
#> [63] "rank"                      "weighted_identity"
#> [65] "lxr_locAcc_currStat_120"   "not_for_annotation"
#> [67] "consensus_splices"         "exon_identity"
#> [69] "identity"                  "idty"
#> [71] "matches"                   "product_coverage"
#> [73] "splices"                   "Gap"
#> [75] "merge_aligner"             "map"
#> [77] "part"                      "lxr_locAcc_currStat_35"
#> [79] "direction"                 "rpt_unit_range"
#> [81] "exon_number"               "number"
#> [83] "allele"                    "align_id"
#> [85] "batch_id"                  "crc32"
#> [87] "curated_alignment"         "promoted_rank"
#> [89] "qtaxid"                    "Is_circular"
#> [91] "country"                   "isolation-source"
#> [93] "note"                      "tissue-type"
#> [95] "codons"                    "transl_table"

# Types that map to `gene_id`:
#> [1] "enhancer"             "gene"
#> [2] "promoter"             "pseudogene"
#> [5] "recombination_region" "sequence_feature"



.standardizeRefSeqToEnsembl <- function(object) {
    assert(is(object, "GRanges"))
    # Rename `gene` column to `gene_id`, matching Ensembl spec.
    colnames(mcols(object)) <- sub(
        pattern = "^gene$",
        replacement = "gene_id",
        x = colnames(mcols(object))
    )
    object
}



.makeGenesFromRefSeqGFF3 <- function(object) {
    object <- .standardizeRefSeqToEnsembl(object)

    # Drop rows that contain a parent element.
    keep <- vapply(
        X = mcols(object)[["Parent"]],
        FUN = function(x) {
            identical(x, character(0))
        },
        FUN.VALUE = logical(1L)
    )
    object <- object[keep]

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

    object
}



# Types that map to `transcript_id`:
#  [1] "antisense_RNA"      "exon"
#  [3] "guide_RNA"          "lnc_RNA"
#  [5] "mRNA"               "primary_transcript"
#  [7] "RNase_MRP_RNA"      "RNase_P_RNA"
#  [9] "rRNA"               "scRNA"
# [11] "snoRNA"             "snRNA"
# [13] "telomerase_RNA"     "transcript"
# [15] "vault_RNA"          "Y_RNA"
.makeTranscriptsFromRefSeqGFF3 <- function(object) {
    object <- .standardizeRefSeqToEnsembl(object)

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

    object
}



# WormBase GTF =================================================================
# WormBase identifier fix. WormBase GTF currently imports somewhat malformed,
# and the gene identifiers require additional sanitization to return correctly.
# Some garbage rows containing "Gene:" or "Transcript:" will remain. We need to
# drop these before proceeding. Note that this step needs to be called after
# TxDb strict mode check.



# FIXME Note that before we
# return gene-level ranges from WormBase, we need to drop some additional
# malformed rows. Refer to `.makeGenesFromGFF()`.
.makeGenesFromWormBaseGTF <- function(object) {
    assert(is(object, "GRanges"))
    object <- .makeGenesFromGTF(object)
    keep <- !grepl(pattern = ":", x = mcols(object)[["gene_id"]])
    object <- object[keep]
    object
}



# FIXME Does this get rid of "Transcript:" rows properly?
.makeTranscriptsFromWormBaseGTF <- .makeTranscriptsFromEnsemblGTF



# TxDb =========================================================================
# Check GRanges from GFF loaded with rtracklayer against GenomicFeatures TxDb.
# Consider adding support for exons, CDS, in a future update.
#
# Expected warnings/failures:
# - FlyBase genes from GTF: FBgn0013687. Visual inspection confirms that ranges
#   from GFF are correct, but TxDb reports 258 mismatches (those are incorrect).
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
    # help(topic = "IPosRanges-comparison", package = "IRanges")
    # vignette(topic = "IRangesOverview", package = "IRanges")
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



# FIXME Can we load GENCODE GFF3 using `makeTxDbFromGRanges()`?
# Doesn't work for Ensembl GFF3 but may be fine for GENCODE?

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
