# TODO Check FlyBase
# TODO Check WormBase
# TODO Check UCSC?



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
    gff <- readGFF(file)

    # Ensure mcols are camel case (`geneID`, not `gene_id`)
    gff <- camel(gff)

    # Alternate method (for genes)
    # gr <- gff[gff$type == "gene"]
    gr <- gff[!is.na(gff$geneID)]
    any(duplicated(gr$geneID))

    gr <- gff[!is.na(gff$transcriptID)]
    any(duplicated(gr$transcriptID))

    # Attributes ===============================================================
    attributes <- as.data.frame(mcols(gff))

    # Determine if GFFv3 or GTF (GFFv2)
    if (all(c("ID", "Name") %in% colnames(attributes))) {
        type <- "GFF"
    } else {
        type <- "GTF"
    }

    # Report the source of the gene annotations
    assert_is_subset("source", colnames(attributes))
    if (any(grepl("FlyBase", attributes[["source"]]))) {
        source <- "FlyBase"
    } else if (any(grepl("WormBase", attributes[["source"]]))) {
        source <- "WormBase"
    } else if (any(grepl(
        "ensembl", attributes[["source"]], ignore.case = TRUE
    ))) {
        source <- "Ensembl"
    } else {
        stop("Unknown/unsupported GFF source")
    }

    message(paste(source, type, "detected"))

    # FlyBase: Rename `symbol` to `name`
    if (source == "FlyBase") {
        colnames(attributes) <- gsub(
            pattern = "_symbol$",
            replacement = "_name",
            x = colnames(attributes)
        )
    }

    # `gene_id` and `transcript_id` are always required
    assert_is_subset(
        x = c("gene_id", "transcript_id"),
        y = colnames(attributes)
    )

    if (type == "GTF") {
        # `gene_name` and `transcript_name` are optional, but recommended.
        # This aren't included in WormBase files, for example.
        assert_is_subset(
            x = c("gene_name", "transcript_name"),
            y = colnames(attributes),
            severity = "warning"
        )
        attributes <- attributes %>%
            # Select only the `gene_` and `transcript_` columns.
            # Note that this will also include biotype information.
            .[, grepl("^(gene|transcript)_", colnames(.)), drop = FALSE] %>%
            filter(
                !is.na(!!sym("transcript_id")),
                !is.na(!!sym("gene_id"))
            ) %>%
            unique()
    } else if (type == "GFF") {
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

    # Convert columns to camel case
    attributes <- camel(attributes)
    # Sort the attributes column names, for consistency
    attributes <- attributes[, sort(colnames(attributes))]

    if (format == "genes") {
        # Gene-level attributes
        attributes <- attributes %>%
            # Merge only the `gene*` columns
            .[, grep("^gene", colnames(.))] %>%
            filter(!is.na(!!sym("geneID"))) %>%
            arrange(!!sym("geneID")) %>%
            unique()
        assert_has_no_duplicates(attributes[["geneID"]])
    } else if (format == "transcripts") {
        # Transcript-level attributes
        attributes <- attributes %>%
            filter(!is.na(!!sym("transcriptID"))) %>%
            arrange(!!sym("transcriptID")) %>%
            unique()
        assert_has_no_duplicates(attributes[["transcriptID"]])
    }

    # GRanges from TxDb ========================================================
    if (format == "genes") {
        gr <- genes(txdb)
        # Ensure mcols are camel case
        gr <- camel(gr)

        # GenomicFeatures returns `geneName` instead of `geneID`
        if (type == "GFF") {
            # https://www.biostars.org/p/196367/
            stop(paste(
                "GenomicFeatures returns `geneName` instead of `geneID`",
                "with Ensembl GFFv3"
            ))
        }

        # Assign gene ID as name and sort
        assert_is_subset("geneID", names(mcols(gr)))
        names(gr) <- mcols(gr)[["geneID"]]
        gr <- gr[sort(names(gr))]

        # Merge annotations into mcols
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
        if (source == "Ensembl") {
            # Ensembl GTF returns integer instead of ID for `transcriptID`
            mcols(gr)[["transcriptID"]] <- mcols(gr)[["transcriptName"]]
            mcols(gr)[["transcriptName"]] <- NULL
        }
        # Assign gene ID as name and sort
        assert_is_subset("transcriptID", names(mcols(gr)))
        names(gr) <- mcols(gr)[["transcriptID"]]
        gr <- gr[sort(names(gr))]


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
