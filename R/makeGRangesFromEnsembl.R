#' Make `GRanges` from Ensembl
#'
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
#'
#' @export
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
#' [makeGRangesFromEnsembl()] supports the legacy *Homo sapiens* GRCh37 (release
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
#'
#' @examples
#' ## Genes
#' x <- makeGRangesFromEnsembl("Homo sapiens", level = "genes")
#' summary(x)
#'
#' ## Transcripts
#' x <- makeGRangesFromEnsembl("Homo sapiens", level = "transcripts")
#' summary(x)
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



# Aliases ======================================================================
#' @describeIn makeGRangesFromEnsembl
#' Legacy convenience function that calls [makeGRangesFromEnsembl()] and
#' returns a `tibble` instead of `GRanges`. Note that `GRanges` can also be
#' coercing using [`as.data.frame()`][BiocGenerics::as.data.frame].
#' @export
annotable <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall()
        )
        # Decode run-length encoding in mcols before coercing to tibble.
        # Otherwise Windows users won't get expected atomic columns.
        mcols(gr) <- decode(mcols(gr))
        as_tibble(gr, rownames = NULL)
    }

formals(annotable) <- formals(makeGRangesFromEnsembl)



# Internal =====================================================================
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
