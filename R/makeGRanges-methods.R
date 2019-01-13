# NOTE Switched to using run-length encoding (Rle) instead of factors in v0.8.
#
# NOTE Still intermittently getting the `dbDisconnect` warning:
#
# ```
# Warning message:
#     call dbDisconnect() when finished working with a connection
# ```
#
# Relevant links:
# - https://github.com/Bioconductor/AnnotationHub/issues/1
# - https://github.com/r-dbi/RSQLite/issues/245



#' Make a `GenomicRanges` object
#'
#' @name makeGRanges
#' @inheritParams params
#'
#' @section Broad class definitions:
#'
#' For gene and transcript annotations, a `broadClass` column is added, which
#' generalizes the gene types into a smaller number of semantically-meaningful
#' groups:
#'
#'   - `coding`.
#'   - `noncoding`.
#'   - `pseudo`.
#'   - `small`.
#'   - `decaying`.
#'   - `ig` (immunoglobulin).
#'   - `tcr` (T cell receptor).
#'   - `other`.
#'
#' @section GRCh37 (hg19) legacy annotations:
#'
#' `makeGRangesFromEnsembl` supports the legacy *Homo sapiens* GRCh37 (release
#' 75) build by internally querying the [EnsDb.Hsapiens.v75][] package.
#' Alternatively, the corresponding GTF/GFF file can be loaded directly from
#' GENCODE or Ensembl.
#'
#' [EnsDb.Hsapiens.v75]: https://bioconductor.org/packages/EnsDb.Hsapiens.v75/
#'
#' @param release `integer(1)`.
#'   Ensembl release version (e.g. `90`). If set `NULL`, defaults to the most
#'   recent release available.
#'
#' @return `GRanges`.
#'
#' @seealso
#' - [AnnotationHub](https://bioconductor.org/packages/AnnotationHub/).
#' - [ensembldb](https://bioconductor.org/packages/ensembldb/).
#' - `rtracklayer::import()`.
#' - `GenomicFeatures::makeTxDbFromGFF()`.
#'
#' @examples
#' ## makeGRangesFromEnsembl ====
#' ## Genes
#' x <- makeGRangesFromEnsembl("Homo sapiens", level = "genes")
#' summary(x)
#'
#' ## Transcripts
#' x <- makeGRangesFromEnsembl("Homo sapiens", level = "transcripts")
#' summary(x)
#'
#' ## makeGRangesFromEnsDb ====
#' x <- makeGRangesFromEnsDb("EnsDb.Hsapiens.v75")
#'
#' ## makeGRangesFromGFF ====
#' file <- file.path(basejumpCacheURL, "example.gtf")
#'
#' ## Genes
#' x <- makeGRangesFromGFF(file = file, level = "genes")
#' summary(x)
#'
#' ## Transcripts
#' x <- makeGRangesFromGFF(file = file, level = "transcripts")
#' summary(x)
NULL



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
#' @param object Object that can be coerced to `DataFrame`, containing gene or
#'   transcript annotations. `GRanges` is recommended.
#'
#' @return Named `factor`.
.broadClass <- function(object) {
    assert(is(object, "GRanges"))

    names <- names(object)
    assert(isCharacter(names))

    data <- as_tibble(object)

    # Early return if already defined.
    if ("broadClass" %in% colnames(data)) {
        broad <- data[["broadClass"]]
        names(broad) <- names
        return(broad)
    }

    # Gene name (required).
    assert(isSubset("geneName", colnames(data)))
    geneName <- data[["geneName"]]

    # Biotype (optional).
    # Prioritize transcript over gene, if present.
    biotypeCol <- grep(
        pattern = "biotype$",
        x = colnames(data),
        ignore.case = TRUE,
        value = TRUE
    )
    if (length(biotypeCol) > 0L) {
        biotypeCol <- biotypeCol[[1L]]
        biotype <- data[[biotypeCol]]
    } else {
        # nocov start
        warning("Biotype column is missing.", call. = FALSE)
        biotypeCol <- NULL
        biotype <- NA
        # nocov end
    }

    # Seqname (optional; aka chromosome).
    seqnameCol <- grep(
        pattern = "seqname",
        x = colnames(data),
        ignore.case = TRUE,
        value = TRUE
    )
    if (length(seqnameCol) > 0L) {
        seqnameCol <- seqnameCol[[1L]]
        seqname <- data[[seqnameCol]]
    } else {
        # nocov start
        warning("Seqname (chromosome) column is missing.", call. = FALSE)
        seqnameCol <- NULL
        seqname <- NA
        # nocov end
    }

    message(paste(
        "Defining broadClass using:",
        toString(c("geneName", biotypeCol, seqnameCol))
    ))

    data <- tibble(
        geneName = geneName,
        biotype = biotype,
        seqname = seqname
    )

    broad <- case_when(
        data[["seqname"]] == "MT" ~ "mito",
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
            x = data[["biotype"]]
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
        # Consider using `NA_character_` here instead.
        TRUE ~ "other"
    )

    broad <- as.factor(broad)
    names(broad) <- names
    broad
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
    assert(isString(id))
    assert(isMatchingRegex(x = id, pattern = "^AH[[:digit:]]+$"))
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



# Report the source of the gene annotations.
.gffSource <- function(gff) {
    assert(isSubset("source", colnames(mcols(gff))))
    if (
        any(grepl("FlyBase", mcols(gff)[["source"]]))
    ) {
        "FlyBase"  # nocov
    } else if (
        any(grepl("WormBase", mcols(gff)[["source"]]))
    ) {
        "WormBase"  # nocov
    } else if (
        any(grepl("Ensembl", mcols(gff)[["source"]], ignore.case = TRUE))
    ) {
        "Ensembl"
    } else {
        stop("Unsupported GFF source.")  # nocov
    }
}



# Determine if GFF or GTF.
.gffType <- function(gff) {
    assert(is(gff, "GRanges"))
    gff <- camel(gff)
    if (all(c("id", "name") %in% colnames(mcols(gff)))) {
        "GFF"
    } else {
        "GTF"
    }
}



.makeGRanges <- function(object) {
    assert(
        is(object, "GRanges"),
        hasNames(object)
    )

    # Standardize the metadata columns.
    mcols <- mcols(object)
    mcols <- camel(mcols)
    # Ensure "ID" is always capitalized (e.g. "entrezid").
    colnames(mcols) <- gsub("id$", "ID", colnames(mcols))
    # Use `transcript` instead of `tx` consistently.
    colnames(mcols) <- gsub(
        pattern = "^tx",
        replacement = "transcript",
        x = colnames(mcols)
    )
    # Remove columns that are all NA.
    mcols <- removeNA(mcols)

    # Handle missing `geneName`.
    if (!"geneName" %in% colnames(mcols)) {
        # nocov start
        warning("`geneName` is missing. Using `geneID` instead.")
        assert(isSubset("geneID", colnames(mcols)))
        mcols[["geneName"]] <- mcols[["geneID"]]
        # nocov end
    }

    # Handle missing `transcriptName`.
    if (
        "transcriptID" %in% colnames(mcols) &&
        !"transcriptName" %in% colnames(mcols)
    ) {
        # nocov start
        warning("`transcriptName` is missing. Using `transcriptID` instead.")
        mcols[["transcriptName"]] <- mcols[["transcriptID"]]
        # nocov end
    }

    # Always use `geneName` instead of `symbol`.
    if (all(c("geneName", "symbol") %in% colnames(mcols))) {
        message("Using `geneName` column instead of `symbol`.")
        mcols[["symbol"]] <- NULL
    } else if ("symbol" %in% colnames(mcols)) {
        # nocov start
        message("Renaming `symbol` column to `geneName`.")
        mcols[["geneName"]] <- mcols[["symbol"]]
        mcols[["symbol"]] <- NULL
        # nocov end
    }

    mcols <- lapply(
        X = mcols,
        FUN = function(col) {
            if (!is.atomic(col) || isS4(col)) {
                # `I` inhibits reinterpretation and returns `AsIs` class.
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
        x = c("geneID", "transcriptID"), y = colnames(mcols(object))
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



#' @describeIn makeGRanges
#' Quickly obtain gene and transcript annotations from
#' [Ensembl](https://www.ensembl.org/) using
#' [AnnotationHub](https://bioconductor.org/packages/AnnotationHub/) and
#' [ensembldb](https://bioconductor.org/packages/ensembldb/).
#'
#' Simply specify the desired organism, using the full latin name. For example,
#' we can obtain human annotations with `Homo sapiens`. Optionally, specific
#' Ensembl genome builds (e.g. `GRCh38`) and release versions (e.g. `87`) are
#' supported.
#'
#' Under the hood, this function fetches annotations from AnnotationHub using
#' the ensembldb package. AnnotationHub supports versioned Ensembl releases,
#' back to version 87.
#'
#' Genome build: use "`GRCh38`" instead of "`hg38`" for the genome build, since
#' we're querying Ensembl and not UCSC.
#' @export
makeGRangesFromEnsembl <- function(
    organism,
    level = c("genes", "transcripts"),
    genomeBuild = NULL,
    release = NULL
) {
    message("Making GRanges from Ensembl.")
    assert(isString(organism))
    level <- match.arg(level)
    if (
        identical(tolower(organism), "homo sapiens") &&
        (
            identical(tolower(as.character(genomeBuild)), "grch37") ||
            identical(release, 75L)
        )
    ) {
        id <- "EnsDb.Hsapiens.v75"
        edb <- .getEnsDbFromPackage(package = id)
    } else {
        id <- .getAnnotationHubID(
            organism = organism,
            genomeBuild = genomeBuild,
            ensemblRelease = release
        )
        edb <- .getEnsDbFromAnnotationHub(id = id)
    }
    gr <- makeGRangesFromEnsDb(object = edb, level = level)
    metadata(gr)[["id"]] <- id
    gr
}



#' @describeIn makeGRanges Use specific `EnsDb` object as annotation source.
#'   Alternatively, can pass in an EnsDb package name as a `character(1)`.
#' @export
makeGRangesFromEnsDb <- function(object, level) {
    level <- match.arg(level)

    message("Making GRanges from EnsDb object.")
    userAttached <- .packages()

    # Allow loading of EnsDb package, passed in as a character string.
    if (isString(object)) {
        package <- object
        requireNamespace(package = package)
        object <- get(
            x = package,
            envir = asNamespace(package),
            inherits = FALSE
        )
    }
    assert(is(object, "EnsDb"))

    # Get the genome build from the ensembldb metdata.
    genomeBuild <- metadata(object) %>%
        as_tibble() %>%
        filter(!!sym("name") == "genome_build") %>%
        pull("value")
    assert(isString(genomeBuild))

    # Define the metadata to return.
    metadata <- c(
        .prototypeMetadata,
        list(
            organism = organism(object),
            genomeBuild = genomeBuild,
            ensemblRelease = as.integer(ensemblVersion(object)),
            ensembldb = metadata(object),
            level = level
        )
    )

    message(paste(
        paste(.li, "Organism:", metadata[["organism"]]),
        paste(.li, "Genome Build:", metadata[["genomeBuild"]]),
        paste(.li, "Ensembl Release:", metadata[["ensemblRelease"]]),
        paste(.li, "Level:", metadata[["level"]]),
        sep = "\n"
    ))

    if (level == "genes") {
        gr <- genes(
            x = object,
            order.by = "gene_id",
            return.type = "GRanges"
        )
    } else if (level == "transcripts") {
        tx <- transcripts(
            x = object,
            order.by = "tx_id",
            return.type = "GRanges"
        )

        # Get additional mcols of interest from gene annotations.
        gene <- genes(
            x = object,
            order.by = "gene_id",
            return.type = "GRanges"
        )

        # Join the transcript- and gene-level annotations.
        txData <- mcols(tx)
        geneData <- mcols(gene)
        # Use BiocTibble left_join DataFrame method here.
        data <- left_join(
            x = as_tibble(txData, rownames = "rowname"),
            y = as_tibble(geneData, rownames = NULL),
            by = "gene_id"
        )
        assert(identical(x = txData[["tx_id"]], y = data[["tx_id"]]))

        # Now we can slot back into the transcript mcols.
        mcols(tx) <- as(data, "DataFrame")
        gr <- tx
    }

    metadata(gr) <- metadata
    .forceDetach(keep = userAttached)
    .makeGRanges(gr)
}

formals(makeGRangesFromEnsDb)[["level"]] <-
    formals(makeGRangesFromEnsembl)[["level"]]



#' @describeIn makeGRanges
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2. We recommend
#' using a GTF file instead of a GFF3 file, if possible.
#'
#' The UCSC website has detailed conventions on the GFF3 format, including
#' the metadata columns.
#'
#' Remote URLs and compressed files are supported.
#' @export
makeGRangesFromGFF <- function(
    file,
    level = c("genes", "transcripts")
) {
    message("Making GRanges from GFF/GTF file.")
    # Note that `import` has assert checks for file (see below).
    level <- match.arg(level)

    # Import -------------------------------------------------------------------
    file <- localOrRemoteFile(file)
    gff <- import(file)
    assert(is(gff, "GRanges"))
    gff <- camel(gff)

    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected."))

    # nocov start
    if (
        source %in% c("FlyBase", "WormBase") &&
        type == "GFF"
    ) {
        stop(paste0(
            "Only GTF files are currently supported from ", source, "."
        ))
    }
    # nocov end

    # Always require `geneID` and `transcriptID` columns in file.
    assert(isSubset(x = c("geneID", "transcriptID"), y = colnames(mcols(gff))))

    # Rename `geneSymbol` to `geneName`.
    # This applies to FlyBase and WormBase annotations.
    colnames(mcols(gff)) <- gsub(
        pattern = "Symbol$",
        replacement = "Name",
        x = colnames(mcols(gff))
    )

    # Genes --------------------------------------------------------------------
    gn <- gff
    gn <- gn[!is.na(mcols(gn)[["geneID"]])]
    gn <- gn[is.na(mcols(gn)[["transcriptID"]])]
    if (type == "GFF") {
        # geneName
        assert(isSubset("name", colnames(mcols(gn))))
        mcols(gn)[["geneName"]] <- mcols(gn)[["name"]]
        mcols(gn)[["name"]] <- NULL
        # geneBiotype
        assert(isSubset("biotype", colnames(mcols(gn))))
        mcols(gn)[["geneBiotype"]] <- mcols(gn)[["biotype"]]
        mcols(gn)[["biotype"]] <- NULL
        # Remove extra columns.
        mcols(gn)[["alias"]] <- NULL
        mcols(gn)[["id"]] <- NULL
        mcols(gn)[["parent"]] <- NULL
    }
    assert(hasNoDuplicates(mcols(gn)[["geneID"]]))
    names(gn) <- mcols(gn)[["geneID"]]
    gn <- gn[sort(names(gn))]

    # Stop on missing genes.
    assert(identical(
        x = names(gn),
        y = sort(unique(na.omit(mcols(gff)[["geneID"]])))
    ))

    if (level == "genes") {
        message(paste(length(gn), "gene annotations detected."))
        gr <- gn
    }

    # Transcripts --------------------------------------------------------------
    if (level == "transcripts") {
        tx <- gff
        tx <- tx[!is.na(mcols(tx)[["transcriptID"]])]
        if (type == "GTF") {
            types <- c(
                "pseudogene",
                "rna",
                "transcript"
            )
            tx <- tx[grepl(
                pattern = paste(types, collapse = "|"),
                x = mcols(tx)[["type"]],
                ignore.case = TRUE
            )]
        } else if (type == "GFF") {
            # transcriptName
            assert(isSubset("name", colnames(mcols(tx))))
            mcols(tx)[["transcriptName"]] <- mcols(tx)[["name"]]
            mcols(tx)[["name"]] <- NULL
            # transcriptBiotype
            assert(isSubset("biotype", colnames(mcols(tx))))
            mcols(tx)[["transcriptBiotype"]] <- mcols(tx)[["biotype"]]
            mcols(tx)[["biotype"]] <- NULL
            # geneID
            assert(
                isSubset("parent", colnames(mcols(tx))),
                all(grepl("^gene:", mcols(tx)[["parent"]]))
            )
            mcols(tx)[["geneID"]] <- as.character(mcols(tx)[["parent"]])
            mcols(tx)[["geneID"]] <- gsub(
                pattern = "^gene:",
                replacement = "",
                x = mcols(tx)[["geneID"]]
            )
            # Remove extra columns.
            mcols(tx)[["alias"]] <- NULL
            mcols(tx)[["id"]] <- NULL
            mcols(tx)[["parent"]] <- NULL
        }
        assert(hasNoDuplicates(mcols(tx)[["transcriptID"]]))
        names(tx) <- mcols(tx)[["transcriptID"]]
        tx <- tx[sort(names(tx))]

        # Stop on missing transcripts.
        assert(identical(
            x = names(tx),
            y = sort(unique(na.omit(mcols(gff)[["transcriptID"]])))
        ))

        message(paste(length(tx), "transcript annotations detected."))
        gr <- tx

        # Merge the gene-level annotations (`geneName`, `geneBiotype`).
        geneCols <- setdiff(
            x = colnames(mcols(gn)),
            y = colnames(mcols(gr))
        )
        if (length(geneCols) > 0L) {
            geneCols <- c("geneID", geneCols)
            merge <- merge(
                x = mcols(gr),
                y = mcols(gn)[, geneCols],
                all.x = TRUE,
                by = "geneID"
            )
            rownames(merge) <- merge[["transcriptID"]]
            merge <- merge[sort(rownames(merge)), ]
            assert(identical(
                x = mcols(gr)[["transcriptID"]],
                y = merge[["transcriptID"]]
            ))
            mcols(gr) <- merge
        }
    }

    # Metadata -----------------------------------------------------------------
    organism <- tryCatch(
        expr = organism(gr),
        error = function(e) character()
    )
    metadata(gr) <- c(
        .prototypeMetadata,
        list(
            file = file,
            level = level,
            organism = organism,
            genomeBuild = character(),
            ensemblRelease = integer()
        )
    )

    .makeGRanges(gr)
}



#' @rdname makeGRanges
#' @usage NULL
#' @export
makeGRangesFromGTF <- makeGRangesFromGFF



#' @describeIn makeGRanges
#' [annotable()] is a legacy convenience function that calls
#' [makeGRangesFromEnsembl()] and returns a `tibble` instead of `GRanges`. Note
#' that `GRanges` can also be coercing using
#' [`as.data.frame()`][BiocGenerics::as.data.frame].
#' @export
annotable <-
    function() {
        gr <- do.call(what = makeGRangesFromEnsembl, args = matchArgsToDoCall())
        # Decode run-length encoding in mcols before coercing to tibble.
        # Otherwise Windows users won't get expected atomic columns.
        mcols(gr) <- decode(mcols(gr))
        as_tibble(gr, rownames = NULL)
    }

formals(annotable) <- formals(makeGRangesFromEnsembl)
