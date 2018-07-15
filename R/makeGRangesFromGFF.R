# TODO Check FlyBase
# TODO Check WormBase
# TODO Check UCSC?
# FIXME Need to figure out how to sanitize geneID from Parent for GFF



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

    # Import GFF as GRanges
    gr <- readGFF(file)
    assert_is_all_of(gr, "GRanges")
    gr <- camel(gr)

    # Determine if GFF or GTF
    if (all(c("ID", "Name") %in% colnames(mcols(gr)))) {
        type <- "GFF"
    } else {
        type <- "GTF"
    }

    # Report the source of the gene annotations
    assert_is_subset("source", colnames(mcols(gr)))
    if (any(grepl("FlyBase", mcols(gr)[["source"]]))) {
        source <- "FlyBase"
    } else if (any(grepl("WormBase", mcols(gr)[["source"]]))) {
        source <- "WormBase"
    } else if (any(grepl(
        "ensembl", mcols(gr)[["source"]], ignore.case = TRUE
    ))) {
        source <- "Ensembl"
    } else {
        stop("Unknown/unsupported GFF source")
    }

    message(paste(source, type, "detected"))

    # Always require `geneID` and `transcriptID` columns in file
    assert_is_subset(
        x = c("geneID", "transcriptID"),
        y = colnames(mcols(gr))
    )

    # FlyBase: Rename `symbol` to `name`
    if (source == "FlyBase") {
        message("Renaming `*Symbol` to `*Name`")
        colnames(mcols(gr)) <- gsub(
            pattern = "Symbol$",
            replacement = "Name",
            x = colnames(attributes)
        )
    }

    if (type == "GTF") {
        # `geneName` and `transcriptName` are optional, but recommended.
        # This aren't included in WormBase files, for example.
        assert_is_subset(
            x = c("geneName", "transcriptName"),
            y = colnames(mcols(gr)),
            severity = "warning"
        )
    } else if (type == "GFF") {
        # FIXME This isn't the right method for handling GFF with rtracklayer
        # Transcripts
        # Obtain `gene_id` from the `Parent` column
        transcripts <- attributes %>%
            filter(!is.na(!!sym("transcript_id"))) %>%
            # Name: transcript_name
            select(!!!syms(c(
                "transcript_id",
                "Name",
                "Parent",
                "biotype"
            ))) %>%
            rename(
                transcript_name = !!sym("Name"),
                transcript_biotype = !!sym("biotype")
            ) %>%
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
            select(!!!syms(c(
                "gene_id",
                "Name",
                "biotype"
            ))) %>%
            rename(
                gene_name = !!sym("Name"),
                gene_biotype = !!sym("biotype")
            ) %>%
            unique()

        # Now it's safe to join the transcript and gene data frames
        attributes <- left_join(transcripts, genes, by = "gene_id")
    }

    # GRanges from TxDb ========================================================
    if (format == "genes") {
        gr <- gr[grepl("gene", mcols(gr)[["type"]])]
        gr <- gr[!is.na(mcols(gr)[["geneID"]])]
        assert_has_no_duplicates(mcols(gr)[["geneID"]])
        names(gr) <- mcols(gr)[["geneID"]]
        if (type == "GFF") {
            stop("Need to fix GFF code")
            # FIXME Set these columns as NULL
            # id
            # parent
        }
    } else if (format == "transcripts") {
        gr <- gr[grepl("transcript", mcols(gr)[["type"]])]
        gr <- gr[!is.na(mcols(gr)[["transcriptID"]])]
        assert_has_no_duplicates(mcols(gr)[["transcriptID"]])
        names(gr) <- mcols(gr)[["transcriptID"]]
    }

    .makeGRanges(gr)
}



# Aliases ======================================================================
#' @rdname makeGRangesFromGFF
#' @usage NULL
#' @export
makeGRangesFromGFF -> makeGRangesFromGTF
