#' Make GRanges from Ensembl
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
#' Genome build: use `"GRCh38"` instead of `"hg38"` for the genome build, since
#' we're querying Ensembl and not UCSC.
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
#' @section AnnotationHub queries:
#'
#' Here's how to perform manual, customized AnnotationHub queries.
#'
#' ```
#' library(AnnotationHub)
#' library(ensembldb)
#' ah <- AnnotationHub()
#'
#' # Human ensembldb (EnsDb) records.
#' ahs <- query(
#'     x = ah,
#'     pattern = c(
#'         "Homo sapiens",
#'         "GRCh38",
#'         "Ensembl",
#'         "EnsDb"
#'     )
#' )
#' mcols(ahs)
#' print(ahs)
#' # EnsDb (Ensembl GRCh38 94; 2018-10-11)
#' ah[["AH64923"]]
#'
#' # Human UCSC TxDb records.
#' ahs <- query(
#'     x = ah,
#'     pattern = c(
#'         "Homo sapiens",
#'         "UCSC",
#'         "TxDb",
#'         "knownGene"
#'     )
#' )
#' mcols(ahs)
#' print(ahs)
#' # TxDb (UCSC hg38 GENCODE 24; 2016-12-22)
#' ah[["AH52260"]]
#' ```
#'
#' @export
#' @note Updated 2020-01-20.
#'
#' @inheritParams acidroxygen::params
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
    release = NULL,
    ignoreTxVersion = TRUE
) {
    assert(
        hasInternet(),
        isString(organism),
        isString(genomeBuild, nullOK = TRUE),
        isInt(release, nullOK = TRUE),
        isFlag(ignoreTxVersion)
    )
    level <- match.arg(level)
    cli_alert("Making GRanges from Ensembl.")
    ## Remap UCSC genome build to Ensembl automatically, if necessary.
    if (isString(genomeBuild)) {
        remap <- tryCatch(
            expr = convertUCSCBuildToEnsembl(genomeBuild),
            error = function(e) NULL
        )
        if (hasLength(remap)) {
            ucsc <- names(remap)
            ensembl <- unname(remap)
            cli_alert_note(sprintf(
                fmt = paste(
                    "Remapping genome build from UCSC ({.val %s}) to",
                    "Ensembl ({.val %s})."
                ),
                ucsc, ensembl
            ))
            genomeBuild <- ensembl
            rm(remap, ucsc, ensembl)
        }
    }
    ## Match user input to EnsDb.
    if (
        identical(tolower(organism), "homo sapiens") &&
        (
            identical(tolower(as.character(genomeBuild)), "grch37") ||
            identical(release, 75L)
        )
    ) {
        ## Legacy support for GRCh37 (hg19).
        id <- "EnsDb.Hsapiens.v75"
        edb <- .getEnsDbFromPackage(package = id)
    } else {
        id <- .getAnnotationHubID(
            organism = organism,
            genomeBuild = genomeBuild,
            release = release
        )
        edb <- .getEnsDbFromAnnotationHub(id = id)
    }
    out <- makeGRangesFromEnsDb(
        object = edb,
        level = level,
        ignoreTxVersion = ignoreTxVersion
    )
    metadata(out)[["id"]] <- id
    out
}



## Aliases =====================================================================
#' @describeIn makeGRangesFromEnsembl
#' Legacy convenience function that calls [makeGRangesFromEnsembl()] and returns
#' a `tbl_df` (tibble) instead of `GRanges`. Note that `GRanges` can also be
#' coercing using [`as.data.frame()`][BiocGenerics::as.data.frame].
#' @export
annotable <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall()
        )
        ## Decode run-length encoding in mcols before coercion. Otherwise,
        ## Windows users won't get expected atomic columns.
        mcols(gr) <- decode(mcols(gr))
        as_tibble(gr, rownames = NULL)
    }

formals(annotable) <- formals(makeGRangesFromEnsembl)



## Internal ====================================================================
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
#' @note Updated 2019-08-21.
#' @noRd
#' @examples .getAnnotationHubID("Homo sapiens")
.getAnnotationHubID <- function(
    organism,
    genomeBuild = NULL,
    release = NULL,
    ah = NULL
) {
    assert(
        isString(organism),
        isString(genomeBuild, nullOK = TRUE),
        isInt(release, nullOK = TRUE)
    )
    userAttached <- .packages()
    ## Standardize organism name, if necessary.
    organism <- gsub(pattern = "_", replacement = " ", x = makeNames(organism))
    ## ensembldb always uses two words for organisms, instead of matching the
    ## Ensembl name exactly. This can mismatch with some organisms. For example,
    ## the dog genome is named "Canis lupus familiaris" on Ensembl but matches
    ## against "Canis familiaris" only with ensembldb. Check for this rare edge
    ## case and inform the user.
    pattern <- "^([a-z]+)\\s[a-z]+\\s([a-z]+)$"
    if (isTRUE(grepl(pattern = pattern, x = organism, ignore.case = TRUE))) {
        fullOrganism <- organism
        organism <- sub(
            pattern = pattern,
            replacement = "\\1 \\2",
            x = fullOrganism,
            ignore.case = TRUE
        )
        cli_alert(sprintf(
            "Matching {.val %s} using {.val %s}.",
            fullOrganism, organism
        ))
    }
    ## Coerce integerish (e.g. 90) to integer (e.g. 90L).
    if (isInt(release)) {
        release <- as.integer(release)
    }
    ## Error on request of unsupported legacy Ensembl release.
    if (
        is.integer(release) &&
        release < 87L
    ) {
        stop("ensembldb currently only supports Ensembl releases >= 87.")
    }
    ## Get AnnotationHub if necessary.
    if (is.null(ah)) {
        ah <- .annotationHub()
    }
    ## Matching EnsDb objects from ensembldb by default.
    rdataclass <- "EnsDb"
    cli_alert(sprintf(
        "Matching {.var %s} from {.pkg AnnotationHub} %s (%s).",
        rdataclass,
        packageVersion("AnnotationHub"),
        snapshotDate(ah)
    ))
    ## Query AnnotationHub.
    ahs <- query(
        x = ah,
        pattern = c(
            "Ensembl",
            organism,
            genomeBuild,
            release,
            rdataclass
        ),
        ignore.case = TRUE
    )
    assert(is(ahs, "AnnotationHub"))
    ## Get the AnnotationHub from the metadata columns.
    mcols <- mcols(ahs, use.names = TRUE)
    ## Sort the entries by title instead of AH identifier.
    ## Updates can otherwise mess up the expected order, for example:
    ## > AH73881 | Ensembl 97 EnsDb for Homo sapiens
    ## > AH73986 | Ensembl 79 EnsDb for Homo sapiens
    mcols <- mcols[order(mcols[["title"]]), , drop = FALSE]
    ## Abort if there's no match and working offline.
    if (!isTRUE(hasInternet()) && !hasRows(mcols)) {
        ## nocov start
        stop("AnnotationHub requires an Internet connection.")
        ## nocov end
    }
    ## Ensure genome build matches, if specified.
    if (!is.null(genomeBuild)) {
        assert(isSubset("genome", colnames(mcols)))
        keep <- which(mcols[["genome"]] %in% genomeBuild)
        mcols <- mcols[keep, , drop = FALSE]
    }
    ## Ensure Ensembl release matches, or pick the latest one.
    if (!is.null(release)) {
        assert(isSubset("title", colnames(mcols)))
        keep <- which(grepl(paste("Ensembl", release), mcols[["title"]]))
        mcols <- mcols[keep, , drop = FALSE]
        assert(hasLength(nrow(mcols), n = 1L))
    }
    ## Error if filtering was unsuccessful.
    if (!hasRows(mcols)) {
        stop(sprintf(
            fmt = paste(
                "No ID matched on AnnotationHub %s.",
                "  - Organism: %s",
                "  - Genome build: %s",
                "  - Ensembl release: %s",
                sep = "\n"
            ),
            packageVersion("AnnotationHub"),
            deparse(organism),
            deparse(genomeBuild),
            deparse(release)
        ))
    }
    ## Select the most recent database (sorted by title, not ID!).
    mcols <- tail(mcols, n = 1L)
    id <- rownames(mcols)
    assert(
        isString(id),
        unname(isMatchingRegex(x = id, pattern = "^AH[[:digit:]]+$"))
    )
    cli_text(sprintf("%s: %s", id, mcols[["title"]]))
    .forceDetach(keep = userAttached)
    id
}



#' Get EnsDb from AnnotationHub
#' @noRd
#' @examples .getEnsDbFromAnnotationHub("AH64923")
.getEnsDbFromAnnotationHub <- function(id, ah = NULL) {
    userAttached <- .packages()
    ## Get AnnotationHub if necessary.
    if (is.null(ah)) {
        ah <- .annotationHub()
    }
    assert(is(ah, "AnnotationHub"))
    ## This step will also output `txProgressBar` on a fresh install. Using
    ## `capture.output` here again to suppress console output.
    ## Additionally, it attaches ensembldb and other Bioconductor dependency
    ## packages, which will mask some tidyverse functions (e.g. `select`).
    invisible(capture.output(
        edb <- suppressMessages(ah[[id]])
    ))
    assert(is(edb, "EnsDb"))
    .forceDetach(keep = userAttached)
    edb
}



#' Get EnsDb from Package
#'
#' @note Updated 2020-01-20.
#' @noRd
#'
#' @examples .getEnsDbFromPackage("EnsDb.Hsapiens.v75")
.getEnsDbFromPackage <- function(package) {
    cli_alert(sprintf("Getting {.var EnsDb} from {.pkg %s}.", package))
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
