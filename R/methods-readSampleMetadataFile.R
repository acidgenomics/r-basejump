#' Read Sample Metadata File
#'
#' @rdname readSampleMetadataFile
#' @name readSampleMetadataFile
#' @family Data Import and Project Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams saveData
#'
#' @param object Metadata file. Supports CSV and XLSX file formats.
#' @param lanes *Optional*. Number of lanes used to split the samples into
#'   technical replicates (`_LXXX`) suffix.
#'
#' @return [data.frame].
#'
#' @examples
#' # Demultiplexed
#' demultiplexed <- file.path(
#'     "http://basejump.seq.cloud",
#'     "sample_metadata",
#'     "demultiplexed.xlsx")
#' readSampleMetadataFile(demultiplexed)
#'
#' # Multiplexed (e.g. inDrop single-cell RNA-seq)
#' multiplexed <- file.path(
#'     "http://basejump.seq.cloud",
#'     "sample_metadata",
#'     "multiplexed.xlsx")
#' readSampleMetadataFile(multiplexed)
NULL



# Constructors =================================================================
#' @importFrom dplyr group_by left_join mutate mutate_all mutate_if
#'   rename ungroup
#' @importFrom rlang .data sym !!
#' @importFrom stringr str_pad
#' @importFrom tidyr expand
.readSampleMetadataFile <- function(
    object,
    lanes = 1L,
    quiet = FALSE) {
    metadata <- readFileByExtension(object, quiet = quiet)

    # Check for manually defined `sampleID`. Warn and remove if present.
    if ("sampleID" %in% colnames(metadata)) {
        warning(paste(
            "'sampleID' should not be manually defined",
            "in the sample metadata file"
        ), call. = FALSE)
        metadata[["sampleID"]] <- NULL
    }

    # Warn on legacy `samplename` column. We need to work on improving the
    # consistency in examples or the internal handlng of file and sample
    # names in a future update.
    if ("samplename" %in% colnames(metadata)) {
        warning(paste(
            "'samplename' is used in some bcbio examples for FASTQ file names,",
            "and 'description' for sample names. Here we are using 'fileName'",
            "for FASTQ file names (e.g. 'control_replicate_1.fastq.gz'),",
            "'description' for multiplexed per file sample names",
            "(e.g. 'control replicate 1', and 'sampleName' for multiplexed",
            "sample names (i.e. inDrop barcoded samples)."
        ), call. = FALSE)
        metadata <- rename(metadata, fileName = .data[["samplename"]])
    }

    # Check for basic required columns
    requiredCols <- c("fileName", "description")
    if (!all(requiredCols %in% colnames(metadata))) {
        stop(paste(
            "Required columns:", toString(requiredCols)
        ), call. = FALSE)
    }

    # Determine whether the samples are multiplexed, based on the presence
    # of duplicate values in the `description` column
    if (any(duplicated(metadata[["fileName"]])) |
        "sequence" %in% colnames(metadata)) {
        multiplexed <- TRUE
    } else {
        multiplexed <- FALSE
    }

    if (isTRUE(multiplexed)) {
        requiredCols <- c("fileName", "description", "sampleName", "sequence")
        if (!all(requiredCols %in% colnames(metadata))) {
            stop(paste(
                "Required columns:", toString(requiredCols)
            ), call. = FALSE)
        }
        # Ensure `sampleName` is unique
        if (any(duplicated(metadata[["sampleName"]]))) {
            stop("'sampleName' column must be unique for multiplexed samples",
                 call. = FALSE)
        }
    } else {
        # Ensure `description` is unique
        if (any(duplicated(metadata[["description"]]))) {
            stop("'description' column must be unique for demultiplexed files",
                 call. = FALSE)
        }
        # Check for user-defined `description` and `sampleName`
        if (all(c("description", "sampleName") %in% colnames(metadata))) {
            stop(paste(
                "Specify only 'description' and omit 'sampleName' for",
                "demultiplexed FASTQ file metadata"
            ), call. = FALSE)
        }
        metadata[["sampleName"]] <- metadata[["description"]]
    }

    metadata <- metadata %>%
        # Valid rows must contain `description` and `sampleName`. Imported Excel
        # files can contain empty rows, so this helps correct that problem.
        .[!is.na(.[["description"]]), , drop = FALSE] %>%
        .[!is.na(.[["sampleName"]]), , drop = FALSE] %>%
        # Strip all NA rows and columns
        removeNA() %>%
        # Make colnames camelCase
        camel(strict = FALSE)

    # Prepare metadata for lane split replicates. This step will expand rows
    # into the number of desired replicates.
    if (lanes > 1L) {
        metadata <- metadata %>%
            group_by(!!sym("description")) %>%
            # Expand by lane (e.g. "L001")
            expand(
                lane = paste0("L", str_pad(1L:lanes, 3L, pad = "0"))
            ) %>%
            left_join(metadata, by = "description") %>%
            ungroup() %>%
            mutate(
                description = paste(
                    .data[["description"]], .data[["lane"]], sep = "_"),
                sampleName = paste(
                    .data[["sampleName"]], .data[["lane"]], sep = "_")
            )
    }

    # This code is only applicable to multiplexed files used for single-cell
    # RNA-seq analysis. For bcbio single-cell RNA-seq, the multiplexed per
    # sample directories are created by combining the `sampleName` column
    # with the reverse complement (`revcomp`) of the index barcode sequence
    # (`sequence`). This is the current behavior for the inDrop pipeline.
    # Let's check for an ACGT sequence and use the revcomp if there's a
    # match. Otherwise just return the `sampleName` as the `sampleID`.
    if (isTRUE(multiplexed)) {
        detectIndex <-
            grepl(x = metadata[["sequence"]],
                  pattern = "^[ACGT]{6,}") %>%
            all()
        if (isTRUE(detectIndex)) {
            metadata[["revcomp"]] <-
                vapply(metadata[["sequence"]], revcomp, character(1L))
            # Match the sample directories exactly here, using the hyphen.
            # We'll sanitize into valid names using `make.names()` in
            # the final return chain.
            metadata[["sampleID"]] <-
                paste(metadata[["description"]],
                      metadata[["revcomp"]],
                      sep = "-")
        }
    }

    # Default to sanitized `sampleName` column for `sampleID`
    if (!"sampleID" %in% colnames(metadata)) {
        metadata[["sampleID"]] <- metadata[["sampleName"]]
    }

    metadata %>%
        mutate_all(as.factor) %>%
        mutate_if(is.factor, droplevels) %>%
        .prepareSampleMetadata()
}



# Methods ======================================================================
#' @rdname readSampleMetadataFile
#' @export
setMethod(
    "readSampleMetadataFile",
    signature("character"),
    .readSampleMetadataFile)
