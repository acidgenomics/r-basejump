#' Read Sample Metadata File
#'
#' @rdname readSampleMetadataFile
#' @name readSampleMetadataFile
#' @family Data Import and Project Utilities
#'
#' @inheritParams AllGenerics
#' @param object Metadata file. Supports CSV and XLSX file formats.
#' @param pattern *Optional*. Grep pattern to match against sample names.
#' @param patternCol *Optional*. Column in data frame used for pattern
#'   subsetting.
#' @param lanes *Optional*. Number of lanes used to split the samples into
#'   technical replicates (`_LXXX`) suffix.
#'
#' @return [tibble], grouped by `sampleName`.
NULL



# Methods ====
#' @rdname readSampleMetadataFile
#' @export
setMethod("readSampleMetadataFile", "character", function(
    object,
    pattern = NULL,
    patternCol = "sampleName",
    lanes = 1) {
    meta <- readFileByExtension(object)

    # Warn on legacy `samplename` column. We need to work on improving the
    # consistency in examples or the internal handlng of file and sample
    # names in a future update.
    if ("samplename" %in% colnames(meta)) {
        warning(paste(
            "'samplename' is used in some bcbio examples for FASTQ file names,",
            "and 'description' for sample names. Here we are using 'fileName'",
            "for FASTQ file names (e.g. 'control_replicate_1.fastq.gz'),",
            "'description' for multiplexed per file sample names",
            "(e.g. 'control replicate 1', and 'sampleName' for multiplexed",
            "sample names (i.e. inDrop barcoded samples)."
        ), call. = FALSE)
        meta <- dplyr::rename(meta, fileName = .data[["samplename"]])
    }

    # Check for basic required columns
    requiredCols <- c("fileName", "description")
    if (!all(requiredCols %in% colnames(meta))) {
        stop(paste("Required columns:", toString(requiredCols)))
    }

    # Prepare metadata for lane split replicates. This step will expand rows
    # into the number of desired replicates.
    if (lanes > 1) {
        meta <- meta %>%
            group_by(!!sym("description")) %>%
            # Expand by lane (e.g. "L001")
            expand(lane = paste0("L", str_pad(1:lanes, 3, pad = "0"))) %>%
            left_join(meta, by = "description") %>%
            ungroup() %>%
            mutate(description = paste(
                .data[["description"]], .data[["lane"]], sep = "_"))
    }

    # Determine whether the samples are multiplexed, based on the presence
    # of duplicate values in the `description` column
    if (any(duplicated(meta[["description"]]))) {
        multiplexedFASTQ <- TRUE
    } else {
        multiplexedFASTQ <- FALSE
    }

    if (isTRUE(multiplexedFASTQ)) {
        requiredCols <- c("fileName", "description", "sampleName", "sequence")
        if (!all(requiredCols %in% colnames(meta))) {
            stop(paste("Required columns:", toString(requiredCols)))
        }
    } else {
        # Check for duplicate `description` and `sampleName`
        if (all(c("description", "sampleName"))) {
            stop(paste(
                "Specify only 'description' and omit 'sampleName' for",
                "demultiplexed FASTQ file metadata"
            ))
        }
        meta[["sampleName"]] <- meta[["description"]]
    }

    meta <- meta %>%
        # Valid rows must contain `description` and `sampleName`. Imported Excel
        # files can contain empty rows, so this helps correct that problem.
        dplyr::filter(!is.na(.data[["description"]])) %>%
        dplyr::filter(!is.na(.data[["sampleName"]])) %>%
        # Strip all NA rows and columns
        removeNA() %>%
        # Make colnames camelCase
        camel(strict = FALSE)

    # Check that sample names are unique
    if (any(duplicated(meta[["sampleName"]]))) {
        stop("Sample names are not unique", call. = FALSE)
    }

    # Subset by pattern, if desired
    if (!is.null(pattern)) {
        meta <- meta %>%
            dplyr::filter(str_detect(.data[[patternCol]], pattern))
    }

    # Set the `sampleID` column
    if (isTRUE(multiplexedFASTQ)) {
        # The per sample directories are created by combining the
        # `sampleName` column with the reverse complement (`revcomp`) of the
        # index barcode sequence (`sequence`)
        meta <- meta %>%
            mutate(
                revcomp = vapply(.data[["sequence"]], revcomp, character(1)),
                # Match the sample directories exactly here, using the hyphen.
                # We'll sanitize into valid names using `make.names()` in
                # the final return chain.
                sampleID = paste(
                    .data[["description"]],
                    .data[["revcomp"]],
                    sep = "-"))
    } else {
        # For demultiplexed samples, we can just use the `description`
        meta[["sampleID"]] <- meta[["description"]]
    }

    .sampleMetadata(meta)
})
