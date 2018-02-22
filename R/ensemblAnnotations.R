#' Ensembl Annotations
#'
#' Quickly obtain gene and transcript annotations from
#' [Ensembl](http://www.ensembl.org/) using [AnnotationHub].
#'
#' The default recommended approach is to simply specify the desired organism,
#' using the full latin name. For example, we can obtain human annotations with
#' `Homo sapiens` and mouse annotations with `Mus musculus`.
#'
#' Under the hood, the function fetches these annotations from AnnotationHub
#' using the ensembldb package. AnnotationHub supports versioned Ensembl
#' releases, back to version 87.
#'
#' @note Use GRCh38 instead of hg38 for the genome build, since we're
#'   querying Ensembl and not UCSC. Unfortunately, GRCh37 is not currently
#'   suported on AnnotationHub.
#'
#' @family Gene Annotation Utilities
#'
#' @inheritParams general
#' @inheritParams saveData
#'
#' @param organism Default recommended usage is to provide the full latin
#'   organism name as a string.
#' @param format Desired annotation data format, either "`genes`",
#'   "`transcripts`", "`gene2symbol`"., or "`tx2gene`".
#' @param genomeBuild *Optional.* Check to see if a specific genome build is
#'   supported.
#' @param release *Optional.* Ensembl release version. Defaults to the most
#'   current release available on AnnotationHub.
#' @param return Class of the returned object. Can be "`data.frame`",
#'   "`DataFrame`" or "`GRanges`. See `help("genes", "ensembldb")` for
#'   additional information.
#'
#' @return Gene or transcript annotations.
#'
#' @seealso
#' - [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub).
#' - [ensembldb](https://doi.org/doi:10.18129/B9.bioc.ensembldb).
#'
#' @examples
#' ensemblAnnotations("Homo sapiens") %>% glimpse()
ensemblAnnotations <- function(
    organism,
    format = "genes",
    genomeBuild = NULL,
    release = NULL,
    return = "data.frame") {
    assert_is_a_string(organism)
    assert_is_a_string(format)
    assert_is_subset(
        x = format,
        y = c("genes", "transcripts", "gene2symbol", "tx2gene"))
    assertIsAStringOrNULL(genomeBuild)
    assertIsAnImplicitIntegerOrNULL(release)
    if (is.numeric(release)) {
        # AnnotableHub only supports releases 87 and above
        assert_all_are_greater_than_or_equal_to(release, 87L)
    }
    assert_is_a_bool(uniqueSymbol)
    assert_is_a_string(return)
    assert_is_subset(return, c("data.frame", "DataFrame", "GRanges"))

    # Ensure `select()` isn't masked by ensembldb/AnnotationDbi
    userAttached <- .packages()

    # Release version ==========================================================
    # Define the `releasePattern` to query with ensembldb
    if (is.null(release)) {
        releasePattern <- NULL
    } else {
        releasePattern <- paste0("v", release)
    }

    # AnnotationHub ============================================================
    # Connect to AnnotationHub. On a fresh install this will print a
    # txProgressBar to the console. We're using `capture.output()` here
    # to suppress the console output, since it's not very informative and
    # can cluster R Markdown reports.
    invisible(capture.output(
        ah <- suppressMessages(AnnotationHub())
    ))

    inform(paste(
        "Loading Ensembl annotations from AnnotationHub",
        snapshotDate(ah),
        sep = "\n"
    ))

    # Get the AnnotationHub dataset by identifier number
    ahDb <- query(
        ah,
        pattern = c(organism, "EnsDb", releasePattern, genomeBuild),
        ignore.case = TRUE)
    # Get the latest build version
    id <- ahDb %>%
        mcols() %>%
        rownames() %>%
        tail(n = 1L)

    # Abort on organism failure
    if (!length(id)) {
        msg <- paste("Ensembl annotations for", organism)
        if (!is.null(genomeBuild)) {
            msg <- paste(msg, genomeBuild, sep = " : ")
        }
        if (!is.null(release)) {
            msg <- paste(msg, release, sep = " : ")
        }
        msg <- paste(
            msg, "were not found in AnnotationHub",
            packageVersion("AnnotationHub")
        )
        abort(msg)
    }

    # ensembldb ================================================================
    # This step will also output `txProgressBar()` on a fresh install. Using
    # `capture.output()` here again to suppress console output. Additionally, it
    # attaches ensembldb and other Bioconductor dependency packages, which will
    # mask some tidyverse functions (e.g. `select()`).
    invisible(capture.output(
        edb <- suppressMessages(ah[[id]])
    ))

    inform(paste(
        "EnsDB", paste0(id, ":"),
        organism(edb),
        "Ensembl", ensemblVersion(edb)
    ))

    # Now we can force detach ensembldb and other unwanted dependendcies from
    # the search path
    ensembldbAttached <- setdiff(.packages(), userAttached)
    invisible(lapply(
        X = ensembldbAttached,
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
    # Assert that all AnnotationHub/ensembldb packages must detach
    assert_are_identical(.packages(), userAttached)

    # Sanitize return ==========================================================
    if (format == "genes") {
        genes(edb, return.type = return)
    } else if (format == "transcripts") {
        transcripts(edb, return.type = return)
    } else if (format == "gene2symbol") {
        genes(
            edb,
            columns = c("gene_id", "symbol"),
            return.type = return)
    } else if (format == "tx2gene") {
        transcripts(
            edb,
            columns = c("tx_id", "gene_id"),
            return.type = return)
    }
}
