#' Connect to AnnotationHub
#'
#' On a fresh install this will print a txProgressBar to the console. We're
#' using [utils::capture.output()] here to suppress the console output, since
#' it's not very informative and can cluster R Markdown reports.
#'
#' @noRd
.annotationHub <- function() {
    userAttached <- .packages()
    invisible(capture.output(
        ah <- suppressMessages(AnnotationHub())
    ))
    assert(is(ah, "AnnotationHub"))
    .forceDetach(keep = userAttached)
    ah
}



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

    # This step ensures `mcols()` are formatted in camel case.
    object <- camel(object)

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
        warning("GRanges does not contain biotype in mcols().")
        biotypeCol <- NULL
        biotypeData <- NA
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
        seqnamesData <- data[[seqnames]]
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
        geneName = geneNameData,
        # Using singular "seqname" instead of "seqnames" in if/else stack.
        seqname = seqnamesData
    )

    # Mitochondrial gene matching depends on the genome.
    # This is important in particular for single-cell RNA-seq.
    #
    # - *H. sapiens*: "MT-" gene name.
    # - *M. musculus*: "mt-" gene name.
    # - *D. melanogaster*: "mt:" gene name.
    # - *C. elegans*: Can't match by gene name. Match by "MtDNA" chromosome.
    #
    # Note that this might not be perfect for other genomes, so consider
    # atttempting to improve support here in a future update.

    # This step uses `dplyr::case_when()`, which allows for a rowwise vectorized
    # if/else call stack.
    out <- case_when(
        grepl(
            pattern = "^MT",
            x = data[["seqname"]],
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



#' Force Detach Packages
#'
#' ensembldb will attach unwanted packages into the NAMESPACE, which can
#' conflict with tidyverse packages (e.g. dplyr).
#'
#' @noRd
.forceDetach <- function(keep = NULL) {
    detach <- setdiff(.packages(), keep)
    if (length(detach) > 0L) {
        invisible(lapply(
            X = detach,
            FUN = function(name) {
                if (name %in% .packages()) {
                    suppressWarnings(detach(
                        name = paste0("package:", name),
                        unload = TRUE,
                        force = TRUE,
                        character.only = TRUE
                    ))
                }
            }
        ))
    }
    assert(identical(.packages(), keep))
}



#' Get AnnotationHub ID
#' @noRd
#' @examples .getAnnotationHubID("Homo sapiens")
.getAnnotationHubID <- function(
    organism,
    genomeBuild = NULL,
    ensemblRelease = NULL,
    ah = NULL
) {
    userAttached <- .packages()
    assert(isString(organism))
    # Standardize organism name, if necessary.
    organism <- gsub("_", " ", makeNames(organism))
    assert(isString(genomeBuild, nullOK = TRUE))
    # Check for accidental UCSC input and stop, informing user.
    if (isString(genomeBuild)) {
        ucscCheck <- tryCatch(
            expr = convertUCSCBuildToEnsembl(genomeBuild),
            error = function(e) NULL
        )
        if (length(ucscCheck) > 0L) {
            stop(paste(
                "UCSC genome build ID detected.",
                "Use Ensembl ID instead.\n",
                printString(ucscCheck)
            ))
        }
    }
    assert(isInt(ensemblRelease, nullOK = TRUE))
    if (isInt(ensemblRelease)) {
        ensemblRelease <- as.integer(ensemblRelease)
    }

    # Error on request of unsupported legacy Ensembl release.
    if (
        is.integer(ensemblRelease) &&
        ensemblRelease < 87L
    ) {
        stop("ensembldb currently only supports Ensembl releases >= 87.")
    }

    # Get AnnotationHub if necessary.
    if (is.null(ah)) {
        ah <- .annotationHub()
    }

    # Matching EnsDb objects from ensembldb by default.
    rdataclass <- "EnsDb"

    message(paste0(
        "Matching ", rdataclass, " from AnnotationHub ",
        packageVersion("AnnotationHub"),
        " (", snapshotDate(ah), ")."
    ))

    # Query AnnotationHub.
    ahs <- query(
        x = ah,
        pattern = c(
            "Ensembl",
            organism,
            genomeBuild,
            ensemblRelease,
            rdataclass
        ),
        ignore.case = TRUE
    )

    # Get the AnnotationHub from the metadata columns.
    mcols <- mcols(ahs)

    # Abort if there's no match and working offline.
    if (!isTRUE(hasInternet()) && !nrow(mcols)) {
        # nocov start
        stop(
            "AnnotationHub requires an Internet connection for query.",
            call. = FALSE
        )
        # nocov end
    }

    # Ensure genome build matches, if specified.
    if (!is.null(genomeBuild)) {
        assert(isSubset("genome", colnames(mcols)))
        mcols <- mcols[mcols[["genome"]] %in% genomeBuild, , drop = FALSE]
    }

    # Ensure Ensembl release matches, or pick the latest one.
    if (!is.null(ensemblRelease)) {
        assert(isSubset("title", colnames(mcols)))
        mcols <- mcols[
            grepl(paste("Ensembl", ensemblRelease), mcols[["title"]]),
            ,
            drop = FALSE
        ]
        assert(hasLength(nrow(mcols), n = 1L))
    }

    if (!nrow(mcols)) {
        stop(paste(
            paste0(
                "No ID matched on AnnotationHub ",
                packageVersion("AnnotationHub"), "."
            ),
            paste(.li, "Organism:", deparse(organism)),
            paste(.li, "Build:", deparse(genomeBuild)),
            paste(.li, "Release:", deparse(ensemblRelease)),
            sep = "\n"
        ))
    }

    mcols <- tail(mcols, n = 1L)
    id <- rownames(mcols)
    assert(
        isString(id),
        unname(isMatchingRegex(x = id, pattern = "^AH[[:digit:]]+$"))
    )
    message(paste0(id, ": ", mcols[["title"]]))
    .forceDetach(keep = userAttached)
    id
}



#' Get EnsDb from AnnotationHub
#' @noRd
#' @examples .getEnsDbFromAnnotationHub("AH64923")
.getEnsDbFromAnnotationHub <- function(id, ah = NULL) {
    userAttached <- .packages()
    # Get AnnotationHub if necessary.
    if (is.null(ah)) {
        ah <- .annotationHub()
    }
    assert(is(ah, "AnnotationHub"))
    # This step will also output `txProgressBar` on a fresh install. Using
    # `capture.output` here again to suppress console output.
    # Additionally, it attaches ensembldb and other Bioconductor dependency
    # packages, which will mask some tidyverse functions (e.g. `select`).
    invisible(capture.output(
        edb <- suppressMessages(ah[[id]])
    ))
    assert(is(edb, "EnsDb"))
    .forceDetach(keep = userAttached)
    edb
}



#' Get EnsDb from Package
#' @noRd
#' @examples .getEnsDbFromPackage("EnsDb.Hsapiens.v75")
.getEnsDbFromPackage <- function(package) {
    message(paste0("Getting EnsDb from ", package, "."))
    userAttached <- .packages()
    assert(isString(package))
    require(package, character.only = TRUE)
    edb <- get(
        x = package,
        envir = asNamespace(package),
        inherits = FALSE
    )
    assert(is(edb, "EnsDb"))
    .forceDetach(keep = userAttached)
    edb
}



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

    assert(is(object, "GRanges"))
    object
}
