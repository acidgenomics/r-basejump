# FIXME Add FlyBase support
# FIXME Add WormBase support
# FIXME Check UCSC?

# FIXME
# FlyBase needs to rename gene_symbol to gene_name
# colnames(attributes) <- colnames(attributes) %>%
# gsub("Symbol$", "Name", .)

# TODO Ensure `transcript` is used in place of `tx` in all code
# Might need to write some object integrity checks



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
#' @examples
#' x <- makeGRangesFromGFF("http://basejump.seq.cloud/example.gtf")
#' summary(x)
#' as.data.frame(x) %>% glimpse()
#'
#' # Ensembl GTF genes
#' makeGRangesFromGFF(
#'     file = "~/Mus_musculus.GRCm38.87.gtf.gz"
#' )
makeGRangesFromGFF <- function(
    file,
    format = c("genes", "transcripts")
) {
    file <- localOrRemoteFile(file)
    # Require GFF or GTF file extension
    stopifnot(grepl("\\.g[ft]f", file, ignore.case = TRUE))
    format <- match.arg(format)

    # Attributes ===============================================================
    gff <- readGFF(file)

    # Generate a data frame containing gene and transcript ID/name mappings
    attributes <- as.data.frame(mcols(gff))

    ensemblGTFCols <- c(
        "gene_biotype",
        "gene_id",
        "gene_name",
        "gene_source",
        "gene_version",
        "transcript_biotype",
        "transcript_id",
        "transcript_name",
        "transcript_source",
        "transcript_support_level",
        "transcript_version"
    )

    if (all(ensemblGTFCols %in% colnames(attributes))) {
        type <- "Ensembl GTF"
    } else if (all(ensemblGFFCols %in% colnames(attributes))) {
        type <- "Ensembl GFF"
    } else if (all(flybaseGTFCols %in% colnames(attributes))) {
        type <- "FlyBase GTF"
    } else if (all(flybaseGFFCols %in% colnames(attributes))) {
        type <- "FlyBase GFF"
    }

    message(paste(type, "detected"))
    message(printString(colnames(attributes)))

    if (type == "Ensembl GTF") {
        attributes <- attributes %>%
            # Select only the `gene_` and `transcript_` columns.
            # Note that this will also include biotype information.
            .[, grepl("^(gene|transcript)_", colnames(.)), drop = FALSE] %>%
            unique() %>%
            camel()
    } else if (type == "Ensembl GFF") {
        # Transcripts
        # Obtain `gene_id` from the `Parent` column
        transcripts <- attributes %>%
            filter(!is.na(!!sym("transcript_id"))) %>%
            # Name: transcript_name
            select(!!!syms(c(
                "transcript_id", "Name", "Parent"
            ))) %>%
            rename(transcript_name = !!sym("Name")) %>%
            unique()
        # Require that all transcripts have a parent gene
        stopifnot(all(grepl("^gene:", transcripts[["Parent"]])))
        transcripts <- transcripts %>%
            mutate(Parent = gsub("^gene:", "", !!sym("Parent"))) %>%
            rename(gene_id = !!sym("Parent"))

        # Genes
        genes <- attributes %>%
            filter(!is.na(!!sym("gene_id"))) %>%
            # Name: gene_name
            select(!!!syms(c("gene_id", "Name"))) %>%
            rename(gene_name = !!sym("Name")) %>%
            unique()

        # Now it's safe to join the transcript and gene data frames
        attributes <- left_join(transcripts, genes, by = "gene_id")
    } else {
        stop("Not supported yet")
    }

    assert_is_subset(
        x = c("transcriptID", "transcriptName", "geneID", "geneName"),
        y = colnames(attributes)
    )

    # Sort the attributes column names, for consistency
    attributes <- attributes[, sort(colnames(attributes))]

    # GRanges ==================================================================
    # Make transcript database (TxDb object)
    txdb <- suppressWarnings(makeTxDbFromGFF(file))
    message(printString(columns(txdb)))

    # GRanges from TxDb
    if (format == "genes") {
        gr <- genes(txdb)
        # Ensure mcols are camel case
        gr <- camel(gr)
        # Assign gene ID as name and sort
        assert_is_subset("geneID", names(mcols(gr)))
        names(gr) <- mcols(gr)[["geneID"]]
        gr <- gr[sort(names(gr))]
        # Gene-level attributes
        attributes <- attributes %>%
            # Merge only the `gene*` columns
            .[, grep("^gene", colnames(.))] %>%
            filter(!is.na(!!sym("geneID"))) %>%
            arrange(!!sym("geneID")) %>%
            unique()
        assert_has_no_duplicates(attributes[["geneID"]])
        assert_is_subset(
            x = mcols(gr)[["geneID"]],
            y = attributes[["geneID"]]
        )
        # Merge annotations into mcols
        merge <- merge(
            x = mcols(gr),
            y = attributes,
            by = "geneID",
            all.x = TRUE
        )
        assert_are_identical(
            x = mcols(gr)[["geneID"]],
            y = merge[["geneID"]]
        )
        mcols(gr) <- merge
    } else if (format == "transcripts") {
        gr <- transcripts(txdb)
        # Rename `tx_` to `transcript_`
        colnames(mcols(gr)) <- gsub("^tx_", "transcript_", colnames(mcols(gr)))
        # Ensure mcols are camel case
        gr <- camel(gr)
        if (type == "Ensembl GTF") {
            # Ensembl GTF returns integer instead of ID for `transcriptID`
            mcols(gr)[["transcriptID"]] <- mcols(gr)[["transcriptName"]]
        }
        # Assign gene ID as name and sort
        assert_is_subset("transcriptID", names(mcols(gr)))
        names(gr) <- mcols(gr)[["transcriptID"]]
        gr <- gr[sort(names(gr))]
        # Transcript-level attributes
        attributes <- attributes %>%
            filter(!is.na(!!sym("transcriptID"))) %>%
            arrange(!!sym("transcriptID")) %>%
            unique()
        assert_has_no_duplicates(attributes[["transcriptID"]])
        assert_is_subset(
            x = mcols(gr)[["transcriptID"]],
            y = attributes[["transcriptID"]]
        )
        # Merge annotations into mcols
        merge <- merge(
            x = mcols(gr),
            y = attributes,
            by = "transcriptID",
            all.x = TRUE
        )
        assert_are_identical(
            x = mcols(gr)[["transcriptID"]],
            y = merge[["transcriptID"]]
        )
        mcols(gr) <- merge
    }

    .makeGRanges(gr)
}



# Aliases ======================================================================
#' @rdname makeGRangesFromGFF
#' @usage NULL
#' @export
makeGRangesFromGFF -> makeGRangesFromGTF
