#' Ensembl Annotations
#'
#' Quickly obtain gene and transcript annotations from
#' [Ensembl](http://www.ensembl.org/) using
#' [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub).
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
#' @note Use GRCh38 instead of hg38 for the genome build, since we're
#'   querying Ensembl and not UCSC. Unfortunately, GRCh37 is not currently
#'   suported on AnnotationHub.
#'
#' @family Gene Annotation Utilities
#'
#' @importFrom AnnotationHub AnnotationHub query snapshotDate
#' @importFrom BiocGenerics organism
#' @importFrom ensembldb ensemblVersion genes transcripts
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
#' @param broadClass Include broad class definitions, based on `biotype` and/or
#'   gene `symbol` annotations.
#' @param makeNames Convert column names in into "`camel`", "`dotted`", or
#'   "`snake`" case.
#' @param sanitize Improve column names and drop duplicated columns:
#'   - Rename `gene_id` to `ensgene`.
#'   - Rename `tx_id` to `enstxp`.
#'   - Rename `gene_biotype` or `tx_biotype` to `biotype`.
#'   - Rename `entrezid` to `entrez`.
#'   - Drop duplicate `gene_name` for only `symbol`.
#'   - Drop duplicate `tx_name` for only `enstxp`.
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
#' genes <- ensemblAnnotations("Homo sapiens", format = "genes")
#' summary(genes)
#'
#' transcripts <- ensemblAnnotations("Homo sapiens", format = "transcripts")
#' summary(transcripts)
ensemblAnnotations <- function(
    organism,
    format = "genes",
    genomeBuild = NULL,
    release = NULL,
    broadClass = TRUE,
    makeNames = "camel",
    sanitize = TRUE,
    return = "GRanges") {
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
    .assertFormalEnsembldbReturn(return)
    .assertFormalMakeNames(makeNames)
    makeNames <- .getMakeNamesFunction(makeNames)
    assert_is_a_bool(sanitize)

    # Ensure `select()` isn't masked by ensembldb/AnnotationDbi
    userAttached <- .packages()

    # Catch UCSC Genome Build IDs ==============================================
    if (is_a_string(genomeBuild)) {
        map <- c(
            "hg19" = "GRCh37",
            "hg38" = "GRCh38",
            "mm10" = "GRCm38")
        if (genomeBuild %in% names(map)) {
            map <- map[match(genomeBuild, names(map))]
            inform(paste("Remapping genome build", names(map), "to", map))
            genomeBuild <- map
        }
    }

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
        "Fetching Ensembl annotations from AnnotationHub",
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

    # Return ==========================================================
    if (format == "genes") {
        data <- genes(edb, return.type = return)
    } else if (format == "transcripts") {
        data <- transcripts(edb, return.type = return)
    } else if (format == "gene2symbol") {
        data <- genes(
            edb,
            columns = c("gene_id", "symbol"),
            return.type = return)
    } else if (format == "tx2gene") {
        data <- transcripts(
            edb,
            columns = c("tx_id", "gene_id"),
            return.type = return)
    }

    # Broad class definitions
    if (isTRUE(broadClass)) {
        data <- .addBroadClassCol(data)
    }

    # Sanitize columns
    if (isTRUE(sanitize)) {
        data <- .sanitizeAnnotationCols(data)
    }

    makeNames(data)
}



.addBroadClassCol <- function(object) {
    assert_is_any_of(object, c("data.frame", "DataFrame", "GRanges"))
    if (is(object, "GRanges")) {
        data <- mcols(object)
    } else {
        data <- object
    }
    assert_has_colnames(data)

    # Biotype
    assert_any_are_matching_regex(colnames(data), "_biotype$")
    biotypeCol <- grep("_biotype$", colnames(data), value = TRUE)
    assert_is_a_string(biotypeCol)
    biotype <- data[, biotypeCol, drop = TRUE]

    # Symbol
    if ("symbol" %in% colnames(data)) {
        symbol <- data[, "symbol", drop = TRUE]
    } else {
        symbol <- NA
    }

    # Gene/transcript ID (rownames)
    idCol <- biotypeCol %>%
        gsub("_biotype$", "", .) %>%
        paste0(., "_id")
    assert_is_subset(idCol, colnames(data))
    rownames <- data[, idCol, drop = TRUE]

    df <- data.frame(
        biotype = biotype,
        symbol = symbol,
        row.names = rownames,
        stringsAsFactors = FALSE)
    # ensembldb currently only outputs character, so match
    broad <- as.character(.broadClass(df))

    if (is(object, "GRanges")) {
        mcols(object)[["broad_class"]] <- broad
    } else {
        object[["broad_class"]] <- broad
    }

    object
}



.sanitizeAnnotationCols <- function(object) {
    if (is(object, "GRanges")) {
        colnames <- colnames(mcols(object))
    } else {
        colnames <- colnames(object)
    }
    colnames <- colnames %>%
        gsub("^entrezid", "entrez", .) %>%
        gsub("^gene_id$", "ensgene", .) %>%
        gsub("^(gene|tx)_biotype", "biotype", .) %>%
        gsub("^tx_id$", "enstxp", .)
    drop <- c("gene_name", "tx_name")
    if (is(object, "GRanges")) {
        colnames(mcols(object)) <- colnames
        mcols(object) <- mcols(object)[setdiff(colnames, drop)]
    } else {
        colnames(object) <- colnames
        object <- object[, setdiff(colnames, drop), drop = FALSE]
    }
    object
}



# Aliases ======================================================================
# Changed to an aliases in v0.3.2
#' @rdname genes
#' @export
annotable <- function(
    object,
    ...,
    return = "data.frame") {
    ensemblAnnotations(
        organism = object,
        ...,
        return = return)
}
