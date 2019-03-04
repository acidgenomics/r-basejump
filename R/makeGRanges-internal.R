#' Broad Class Definitions
#'
#' @author Rory Kirchner, Michael Steinbaugh
#' @noRd
#'
#' @inheritParams params
#' @param object `GRanges`.
#'
#' @return Named `factor`.
.broadClass <- function(object) {
    assert(is(object, "GRanges"))

    # Early return if already defined in `mcols()`.
    if ("broadClass" %in% colnames(mcols(object))) {
        out <- mcols(object)[["broadClass"]]
        names(out) <- names(object)
        return(out)
    }

    # This step coerces the GRanges to a tibble, which will now contain
    # seqnames, start, end, width, and strand as columns.
    data <- as_tibble(object, rownames = NULL)

    # Biotypes -----------------------------------------------------------------
    # Prioritizing transcript biotype over gene, if defined. This only applies
    # for transcript-level GRanges. For gene-level GRanges, the gene biotypes
    # will be used, as expected.
    if ("transcriptBiotype" %in% colnames(data)) {
        biotypeCol <- "transcriptBiotype"
        biotypeData <- data[[biotypeCol]]
    } else if ("geneBiotype" %in% colnames(data)) {
        biotypeCol <- "geneBiotype"
        biotypeData <- data[[biotypeCol]]
    } else {
        # FlyBase GTF will hit this step.
        # Note that we're early returning without calculations in this case.
        warning(paste(
            "GRanges does not contain biotype in mcols().",
            "Returning without broad class definitions.",
            sep = "\n"
        ))
        # Early `NA` return works successfully in `mcols()` construction.
        return(NA)
    }

    # Gene names ---------------------------------------------------------------
    if ("geneName" %in% colnames(data)) {
        geneNameCol <- "geneName"
        geneNameData <- data[[geneNameCol]]
    } else {
        warning("GRanges does not contain gene names in mcols().")
        geneNameCol <- NULL
        geneNameData <- NA
    }

    # Seqnames -----------------------------------------------------------------
    # This refers to the chromosome name.
    # Note that `as_tibble()` coercion will define `seqnames` column from the
    # `GRanges` object (see above).
    if ("seqnames" %in% colnames(data)) {
        seqnamesCol <- "seqnames"
        seqnamesData <- data[[seqnamesCol]]
    } else {
        # Don't think this is currently possible to hit, but keep just in case.
        warning("GRanges does not contain seqnames.")
        seqnamesCol <- NULL
        seqnamesData <- NA
    }

    # If/else chain ------------------------------------------------------------
    message(paste(
        "Defining broadClass using:",
        # Note that `c()` call here effectively removes `NULL` definitions.
        toString(c(biotypeCol, geneNameCol, seqnamesCol))
    ))
    data <- tibble(
        biotype = biotypeData,
        chromosome = seqnamesData,
        geneName = geneNameData
    )

    # Mitochondrial gene matching depends on the genome.
    # This is important in particular for single-cell RNA-seq.
    #
    # - *H. sapiens*: "MT-" gene name.
    # - *M. musculus*: "mt-" gene name.
    # - *D. melanogaster*: "mt:" gene name.
    # - *C. elegans*: Can't match by gene name.
    #   Match by "MtDNA" chromosome.
    #   Alternatively, can match using "MTCE" sequence name (parent clone).
    #   https://www.wormbase.org/species/c_elegans/clone/MTCE
    #
    # Note that this might not be perfect for other genomes, so consider
    # atttempting to improve support here in a future update.

    # This step uses `dplyr::case_when()`, which allows for a rowwise vectorized
    # if/else call stack.
    out <- case_when(
        grepl(
            pattern = "^MT",
            x = data[["chromosome"]],
            ignore.case = TRUE
        ) ~ "mito",
        grepl(
            pattern = "^mt[\\:\\-]",
            x = data[["geneName"]],
            ignore.case = TRUE
        ) ~ "mito",
        data[["biotype"]] == "protein_coding" ~ "coding",
        data[["biotype"]] %in% c(
            "known_ncrna",
            "lincRNA",
            "non_coding"
        ) ~ "noncoding",
        grepl(
            pattern = "pseudo",
            x = data[["biotype"]],
            ignore.case = TRUE
        ) ~ "pseudo",
        data[["biotype"]] %in% c(
            "miRNA",
            "misc_RNA",
            "ribozyme",
            "rRNA",
            "scaRNA",
            "scRNA",
            "snoRNA",
            "snRNA",
            "sRNA"
        ) ~ "small",
        data[["biotype"]] %in% c(
            "non_stop_decay",
            "nonsense_mediated_decay"
        ) ~ "decaying",
        grepl(
            pattern = "^ig_",
            x = data[["biotype"]],
            ignore.case = TRUE
        ) ~ "ig",
        grepl(
            pattern = "^tr_",
            x = data[["biotype"]],
            ignore.case = TRUE
        ) ~ "tcr",
        # Alternatively, can `NA_character_` here instead. Keeping this as
        # "other", to main consistency with previous data sets. Also note that
        # `NA` can behave inconsistently in plotting engines.
        TRUE ~ "other"
    )

    out <- as.factor(out)
    names(out) <- names(object)
    out
}



# FIXME Need to ensure the levels get dropped on mcols here...
# This isn't returning correctly for `makeGRangesFromGFF()`.
.makeGRanges <- function(object) {
    assert(
        is(object, "GRanges"),
        hasNames(object)
    )

    # Standardize the metadata columns.
    mcols <- mcols(object)
    # Always return using camel case, even though GFF/GTF files use snake.
    mcols <- camel(mcols)
    # Ensure "ID" is always capitalized (e.g. "entrezid").
    colnames(mcols) <- gsub("id$", "ID", colnames(mcols))
    # Use `transcript` instead of `tx` consistently.
    colnames(mcols) <- gsub(
        pattern = "^tx",
        replacement = "transcript",
        x = colnames(mcols)
    )
    # Remove columns that are all `NA`. This step will remove all
    # transcript-level columns from gene-level ranges.
    mcols <- removeNA(mcols)

    # Warn on missing name (symbol) columns.
    if (!"geneName" %in% colnames(mcols)) {
        warning("GRanges does not contain `geneName` in mcols().")
    }
    if (
        "transcriptID" %in% colnames(mcols) &&
        !"transcriptName" %in% colnames(mcols)
    ) {
        warning("GRanges does not contain `transcriptName` in mcols().")
    }

    # Always use `geneName` instead of `symbol`.
    # Note that ensembldb output duplicates these.
    if (all(c("geneName", "symbol") %in% colnames(mcols))) {
        mcols[["symbol"]] <- NULL
    } else if ("symbol" %in% colnames(mcols)) {
        message("Renaming `symbol` column to `geneName` in mcols().")
        mcols[["geneName"]] <- mcols[["symbol"]]
        mcols[["symbol"]] <- NULL
    }

    mcols <- lapply(
        X = mcols,
        FUN = function(col) {
            if (!is.atomic(col) || isS4(col)) {
                # `I()` inhibits reinterpretation and returns `AsIs` class.
                # This keeps complex columns (e.g. Entrez list) intact.
                # Recommended in the `DataFrame` documentation.
                I(col)
            } else {
                # Check to see if any character columns containing repeated
                # values should be coerced to factor first (e.g. geneBiotype).
                if (is.character(col) && any(duplicated(col))) {
                    col <- as.factor(col)
                }
                # Use S4 run length encoding (Rle) for atomic metadata columns.
                # Many of these elements are repetitive, and this makes
                # operations faster.
                Rle(col)
            }
        }
    )
    mcols <- as(mcols, "DataFrame")
    mcols(object) <- mcols

    # Require that names match the identifier column.
    # Use `transcriptID` over `geneID` if defined.
    assert(areIntersectingSets(
        x = c("geneID", "transcriptID"),
        y = colnames(mcols(object))
    ))
    if ("transcriptID" %in% colnames(mcols(object))) {
        idCol <- "transcriptID"
    } else {
        idCol <- "geneID"
    }
    names(object) <- mcols(object)[[idCol]]

    # Ensure broad class definitions are included.
    mcols(object)[["broadClass"]] <- Rle(.broadClass(object))

    # Sort metadata columns alphabetically.
    mcols(object) <- mcols(object)[, sort(colnames(mcols(object)))]

    # Ensure GRanges is sorted by names.
    message(paste0("Arranging by ", idCol, "."))
    object <- object[sort(names(object))]

    # Prepare the metadata.
    # Slot organism into metadata.
    object <- .slotOrganism(object)
    # Ensure object contains prototype metadata.
    metadata(object) <- c(
        .prototypeMetadata,
        metadata(object)
    )

    assert(is(object, "GRanges"))
    object
}



# Slot organism into `metadata()`, if necessary.
.slotOrganism <- function(object) {
    if (is.null(metadata(object)[["organism"]])) {
        metadata(object)[["organism"]] <- tryCatch(
            expr = organism(object),
            error = function(e) character()
        )
    }
    object
}
