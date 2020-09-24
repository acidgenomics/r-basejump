#' Get EnsDb from AnnotationHub
#'
#' @export
#' @note Updated 2020-09-24.
#'
#' @inheritParams acidroxygen::params
#' @param release `integer(1)`.
#'   Ensembl release version (e.g. `90`). If set `NULL`, defaults to the most
#'   recent release available.
#'
#' @return `EnsDb`.
#'
#' @examples
#' edb <- getEnsDbFromAnnotationHub(
#'     organism = "Homo sapiens",
#'     release = 100L,
#'     build = "GRCh38"
#' )
#' print(edb)
getEnsDb <- function(
    organism,
    genomeBuild = NULL,
    release = NULL
) {
    assert(
        hasInternet(),
        isString(organism),
        isString(genomeBuild, nullOK = TRUE),
        isInt(release, nullOK = TRUE)
    )
    ## Remap UCSC genome build to Ensembl automatically, if necessary.
    if (isString(genomeBuild)) {
        remap <- tryCatch(
            expr = convertUCSCBuildToEnsembl(genomeBuild),
            error = function(e) NULL
        )
        if (hasLength(remap)) {
            ucsc <- names(remap)
            ensembl <- unname(remap)
            cli_alert_warning(sprintf(
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
    edb
}



#' Connect to AnnotationHub
#'
#' @note Updated 2020-09-24.
#' @noRd
#'
#' @details
#' On a fresh install this will print a txProgressBar to the console. We're
#' using [utils::capture.output()] here to suppress the console output, since
#' it's not very informative and can cluster R Markdown reports.
.annotationHub <- function() {
    userAttached <- .packages()
    invisible(capture.output({
        ah <- suppressMessages(AnnotationHub())
    }))
    assert(is(ah, "AnnotationHub"))
    .forceDetach(keep = userAttached)
    ah
}



#' Get the AnnotationHub ID for desired EnsDb
#'
#' @note Updated 2020-09-24.
#' @noRd
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
    ## Get AnnotationHub.
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
    cli_alert_info(sprintf("{.val %s}: %s.", id, mcols[["title"]]))
    .forceDetach(keep = userAttached)
    id
}



#' Get EnsDb from AnnotationHub
#'
#' @note Updated 2020-09-24.
#' @noRd
#'
#' @details
#' This step will also output `txProgressBar` on a fresh install. Using
#' `capture.output` here again to suppress console output. Additionally, it
#' attaches ensembldb and other Bioconductor dependency packages, which will
#' mask some tidyverse functions (e.g. `select`).
#'
#' @examples
#' .getEnsDbFromAnnotationHub("AH64923")
.getEnsDbFromAnnotationHub <- function(id, ah = NULL) {
    assert(isString(id))
    userAttached <- .packages()
    if (is.null(ah)) {
        ah <- .annotationHub()
    }
    assert(is(ah, "AnnotationHub"))
    invisible(capture.output({
        edb <- suppressMessages(ah[[id]])
    }))
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
