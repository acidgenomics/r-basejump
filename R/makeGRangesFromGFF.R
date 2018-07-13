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
makeGRangesFromGFF <- function(
    file,
    format = c("genes", "transcripts")
) {
    file <- localOrRemoteFile(file)
    format <- match.arg(format)

    isGTF <- grepl("\\.gtf", file, ignore.case = TRUE)
    if (isGTF) {
        message("GTF (GFFv2) file detected")
    } else {
        message("GFF3 file detected")
    }

    # Attributes ===============================================================
    # GenomicFeatures doesn't support returning the `geneName` from the key
    # value pairs of the GFF file.
    # Require that GFF3 or GTF files have names (symbols).
    # Note that GFF3 has separate rows for "Name" (e.g. geneName, transcriptName).
    # FIXME Need to add FlyBase GFF3 support and unit test
    gff <- readGFF(file)

    # Generate a data frame containing gene and transcript ID/name mappings
    attributes <- as.data.frame(gff)
    message(printString(colnames(attributes)))

    if (isGTF) {
        # Ensembl GTF
        # FIXME Don't do this...we need to keep the biotype info
        attributes <- attributes %>%
            select(!!!syms(c(
                "transcript_id",
                "transcript_name",
                "gene_id",
                "gene_name"
            ))) %>%
            unique() %>%
            camel()
        # Standardize columns into Ensembl format
        colnames(attributes) <- colnames(attributes) %>%
            gsub("Symbol$", "Name", .)
    } else {
        # Ensembl GFF

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
    }

    # FIXME Update now only returns these columns for GFF/GTF
    assert_are_subset(
        x = c("transcriptID", "transcriptName", "geneID", "geneName"),
        y = colnames(attributes)
    )

    # GRanges ==================================================================
    # Make TxDb
    txdb <- suppressWarnings(makeTxDbFromGFF(file))

    # GRanges from TxDb
    if (format == "genes") {
        # FIXME GFF3 is returning geneName instead of geneID for names here
        gr <- genes(txdb, columns = "gene_id")
        colnames(mcols(gr)) <- "geneID"
        attributes <- attributes %>%
            # Merge only the `gene*` columns
            .[, grep("^gene", colnames(.))] %>%
            # Drop rows containing an NA value
            .[complete.cases(.), , drop = FALSE] %>%
            unique()
        assert_has_no_duplicates(attributes[["geneID"]])
        assert_is_subset(
            x = mcols(gr)[["geneID"]],
            y = attributes[["geneID"]]
        )
        merge <- merge(
            x = mcols(gr),
            y = attributes,
            by = "geneID",
            all.x = TRUE
        )
        mcols(gr) <- merge
        assert_are_identical(
            x = names(gr),
            y = mcols(gr)[["geneID"]]
        )
    } else if (format == "transcripts") {
        # `transcript_id` returns as integer, so use `transcript_name` instead
        # and rename
        gr <- transcripts(txdb, columns = "transcript_name")
        colnames(mcols(gr)) <- "transcriptID"
        # Need to set the names on the GRanges object manually
        names(gr) <- mcols(gr)[["transcriptID"]]
        # Order GRanges by `transcriptID`
        gr <- gr[sort(names(gr))]
        # Merge the attributes columns
        attributes <- attributes %>%
            # Merge only the `gene*` and `transcript*` columns
            .[, grep("^(gene|transcript)", colnames(.)), drop = FALSE] %>%
            # Drop rows containing an NA value
            .[complete.cases(.), , drop = FALSE] %>%
            unique()
        assert_has_no_duplicates(attributes[["transcriptID"]])
        assert_is_subset(
            x = mcols(gr)[["transcriptID"]],
            y = attributes[["transcriptID"]]
        )
        merge <- merge(
            x = mcols(gr),
            y = attributes,
            by = "transcriptID",
            all.x = TRUE
        )
        mcols(gr) <- merge
        assert_are_identical(
            x = names(gr),
            y = mcols(gr)[["transcriptID"]]
        )
    }

    .makeGRanges(gr)
}



# Aliases ======================================================================
#' @rdname makeGRangesFromGFF
#' @usage NULL
#' @export
makeGRangesFromGFF -> makeGRangesFromGTF
