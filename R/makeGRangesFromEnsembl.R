#' Genomic Ranges from Ensembl
#'
#' Quickly obtain gene and transcript annotations from
#' [Ensembl](http://www.ensembl.org) using
#' [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub) and
#' [ensembldb](https://doi.org/doi:10.18129/B9.bioc.ensembldb).
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
#' @section Broad class definitions:
#' For gene and transcript annotations, a `broadClass` column is added, which
#' generalizes the gene types into a smaller number of semantically-meaningful
#' groups:
#'
#'   - `coding`
#'   - `noncoding`
#'   - `pseudo`
#'   - `small`
#'   - `decaying`
#'   - `ig` (immunoglobulin)
#'   - `tcr` (T cell receptor)
#'   - `other`
#'
#' @section GRCh37 legacy annotations:
#' The *Homo sapiens* GRCh37 (release 75) build is supported by internally
#' querying the
#' [EnsDb.Hsapiens.v75](https://doi.org/doi:10.18129/B9.bioc.EnsDb.Hsapiens.v75)
#' package.
#'
#' @note Use "`GRCh38`" instead of "`hg38`" for the genome build, since we're
#'   querying Ensembl and not UCSC.
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param format `string`. Fetch ranges as "`genes`" or "`transcripts`".
#' @param genomeBuild `string` or `NULL`. Genome build assembly name (e.g.
#'   "`GRCh38`"). If set `NULL`, defaults to the most recent build available.
#' @param release `scalar integer` or `NULL`. Release version (e.g. `90`). If
#'   set `NULL`, defaults to the most recent release available.
#' @param metadata `boolean`. Include the AnnotationHub metadata inside a
#'   `list`.
#'
#' @return `GRanges`.
#' @export
#'
#' @seealso
#' - [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub).
#' - [ensembldb](https://doi.org/doi:10.18129/B9.bioc.ensembldb).
#'
#' @examples
#' # Genes ====
#' x <- makeGRangesFromEnsembl("Homo sapiens", format = "genes")
#' summary(x)
#'
#' # Transcripts ====
#' x <- makeGRangesFromEnsembl("Homo sapiens", format = "transcripts")
#' summary(x)
makeGRangesFromEnsembl <- function(
    organism,
    format = c("genes", "transcripts"),
    genomeBuild = NULL,
    release = NULL,
    metadata = FALSE
) {
    assert_is_a_string(organism)
    # Standard organism query, if necessary
    organism <- gsub("_", " ", makeNames(organism))
    format <- match.arg(format)
    assertIsAStringOrNULL(genomeBuild)
    # Check for UCSC genome build and warn
    if (is_a_string(genomeBuild)) {
        genomeBuildRemap <- convertUCSCBuildToEnsembl(genomeBuild)
        if (!is.null(genomeBuildRemap)) {
            warning(paste(
                "UCSC genome build ID detected. Use Ensembl build ID instead.",
                paste("Mapping:", deparse(genomeBuildRemap)),
                sep = "\n"
            ))
            # Coerce to drop any UCSC to Ensembl key mapping in name
            genomeBuild <- as.character(genomeBuildRemap)
        }
    }
    assertIsAnImplicitIntegerOrNULL(release)
    if (isAnImplicitInteger(release)) {
        # Note that ensembldb currently only supports >= 87
        assert_all_are_positive(release)
        release <- as.integer(release)
    }
    assert_is_a_bool(metadata)

    # Ensure `select()` isn't masked by ensembldb/AnnotationDbi
    userAttached <- .packages()

    # Fetch annotations from AnnotationHub/ensembldb ===========================
    # ah: AnnotationHub
    # edb: Ensembl database
    if (
        tolower(organism) == "homo sapiens" &&
        (
            identical(tolower(as.character(genomeBuild)), "grch37") ||
            identical(release, 75L)
        )
    ) {
        # GRCh37 release 75
        id <- "EnsDb.Hsapiens.v75"
        if (requireNamespace(id, quietly = TRUE)) {
            edb <- get(id, envir = asNamespace(id), inherits = FALSE)
        } else {
            # nocov start
            stop(paste(
                "GRCh37 genome build requires the", id, "package"
            ))
            # nocov end
        }
    } else {
        # AnnotationHub ========================================================
        # Connect to AnnotationHub. On a fresh install this will print a
        # txProgressBar to the console. We're using `capture.output()` here
        # to suppress the console output, since it's not very informative and
        # can cluster R Markdown reports.
        invisible(capture.output(
            ah <- suppressMessages(AnnotationHub())
        ))

        message(paste(
            "Making GRanges from Ensembl with AnnotationHub",
            packageVersion("AnnotationHub"),
            paste0("(", snapshotDate(ah), ")")
        ))

        # Use ensembldb annotations by default
        rdataclass <- "EnsDb"

        # For legacy release requests, switch to newest version available
        if (!is.null(release) && release < 87L) {
            warning(paste(
                "ensembldb currently only supports Ensembl releases >= 87.",
                "Switching to current release instead.",
                sep = "\n"
            ))
            release <- NULL
        }

        # Query AnnotationHub
        ahdb <- query(
            ah,
            pattern = c("Ensembl", organism, release, genomeBuild, rdataclass),
            ignore.case = TRUE
        )
        mcols <- mcols(ahdb)
        if (!nrow(mcols)) {
            stop(paste(
                paste(
                    "No ID matched on AnnotationHub",
                    packageVersion("AnnotationHub")
                ),
                paste("organism:", deparse(organism)),
                paste("build:", deparse(genomeBuild)),
                paste("release:", deparse(release)),
                sep = "\n"
            ))
        } else {
            # Pick the latest EnsDb that matches
            mcols <- tail(mcols, 1L)
            message(mcols[["title"]])
        }
        id <- rownames(mcols)

        # This step will also output `txProgressBar()` on a fresh install. Using
        # `capture.output()` here again to suppress console output.
        # Additionally, it attaches ensembldb and other Bioconductor dependency
        # packages, which will mask some tidyverse functions (e.g. `select()`).
        invisible(capture.output(
            edb <- suppressMessages(ah[[id]])
        ))
    }

    # Get annotations from EnsDb ===============================================
    assert_is_all_of(edb, "EnsDb")

    meta <- metadata(edb)
    assert_is_data.frame(meta)
    meta <- rbind(c("id", id), meta)
    meta <- as(meta, "tibble")

    # Stash the AnnotationHub ID
    genomeBuild <- meta[meta[["name"]] == "genome_build", "value", drop = TRUE]
    assert_is_a_string(genomeBuild)

    message(paste(
        paste("id:", deparse(id)),
        paste("organism:", deparse(organism(edb))),
        paste("build:", deparse(genomeBuild)),
        paste("release:", deparse(ensemblVersion(edb))),
        paste("format:", deparse(format)),
        sep = "\n"
    ))

    if (format == "genes") {
        gr <- genes(
            x = edb,
            order.by = "gene_id",
            return.type = "GRanges"
        )
    } else if (format == "transcripts") {
        tx <- transcripts(
            x = edb,
            order.by = "tx_id",
            return.type = "GRanges"
        )

        # Get additional mcols of interest from gene annotations
        gene <- genes(
            x = edb,
            order.by = "gene_id",
            return.type = "GRanges"
        )

        # Merge the data
        txData <- mcols(tx)
        geneData <- mcols(gene)
        mergeData <- merge(
            x = txData,
            y = geneData,
            by = "gene_id",
            all.x = TRUE,
            sort = FALSE
        )
        assert_are_set_equal(txData[["tx_id"]], mergeData[["tx_id"]])
        match <- match(x = txData[["tx_id"]], table = mergeData[["tx_id"]])
        stopifnot(!any(is.na(match)))
        mergeData <- mergeData[match, , drop = FALSE]
        assert_are_identical(txData[["tx_id"]], mergeData[["tx_id"]])

        # Now we can slot back into the transcript mcols
        mcols(tx) <- mergeData
        gr <- tx
    }

    # Force detach =============================================================
    fxnAttached <- setdiff(.packages(), userAttached)
    invisible(lapply(
        X = fxnAttached,
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
    assert_are_identical(.packages(), userAttached)

    gr <- .makeGRanges(gr)

    # Include the EnsDB metadata inside a list, if desired
    if (isTRUE(metadata)) {
        list(data = gr, metadata = meta)
    } else {
        gr
    }
}
