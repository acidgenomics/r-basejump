#' Get EnsDb from Bioconductor AnnotationHub
#'
#' @note Updated 2020-09-24.
#' @noRd
#'
#' @details
#' Remaps UCSC genome build to Ensembl automatically, if necessary.
#' Provides legacy support for GRCh37 (hg19).
#'
#' @examples
#' getEnsDb(organism = "Homo sapiens", release = 100L)
getEnsDb <- function(
    organism,
    genomeBuild = NULL,
    release = NULL
) {
    assert(
        isString(organism),
        isString(genomeBuild, nullOK = TRUE),
        isInt(release, nullOK = TRUE)
    )
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
            release = release
        )
        edb <- .getEnsDbFromAnnotationHub(id = id)
    }
    attr(edb, "id") <- id
    edb
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
    assert(
        isString(id),
        is(ah, "AnnotationHub") || is.null(ah)
    )
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
