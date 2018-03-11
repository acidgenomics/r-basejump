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
#' @section Broad Class Definitions:
#' For gene and transcript tables, when `broadClass = TRUE`, a `broadClass`
#' column is added, which generalizes the gene types into a smaller number of
#' semantically-meaningful groups:
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
#' @note Use GRCh38 instead of hg38 for the genome build, since we're
#'   querying Ensembl and not UCSC. Unfortunately, GRCh37 is not currently
#'   suported on AnnotationHub.
#'
#' @name ensembl
#' @family Gene Functions
#'
#' @importFrom AnnotationHub AnnotationHub query snapshotDate
#' @importFrom BiocGenerics organism start
#' @importFrom ensembldb ensemblVersion genes transcripts
#' @importFrom utils packageVersion
#'
#' @inheritParams general
#' @inheritParams saveData
#'
#' @param organism Default recommended usage is to provide the full latin
#'   organism name as a string.
#' @param format Desired annotation data format, either "`genes`",
#'   "`transcripts`", "`gene2symbol`"., or "`tx2gene`".
#' @param genomeBuild *Optional.* Genome assembly name (e.g. GRCh38).
#' @param release *Optional.* Ensembl release version. Defaults to the most
#'   current release available on AnnotationHub.
#' @param metadata Include the AnnotationHub metadata inside a list. Useful
#'   for documenting where the annotations were sourced from inside
#'   AnnotationHub.
#' @param return Class of the returned object. Only applies when `format` equals
#'   "`genes`" or "`transcripts`". Can be "`GRanges`" (default), "`DataFrame`",
#'   or "`data.frame`". See `help("genes", "ensembldb")` for additional
#'   information. "`gene2symbol`" and "`tx2gene`" are always returned as
#'   `data.frame`.
#'
#' @return `GRanges`, `DataFrame`, or `data.frame` containing gene or
#'   transcript annotations.
#' @export
#'
#' @seealso
#' - [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub).
#' - [ensembldb](https://doi.org/doi:10.18129/B9.bioc.ensembldb).
#'
#' @examples
#' # Genes
#' genes <- ensembl("Homo sapiens", format = "genes")
#' summary(genes)
#'
#' # Transcripts
#' transcripts <- ensembl("Homo sapiens", format = "transcripts")
#' summary(transcripts)
#'
#' # Include AnnotationHub metadata
#' x <- ensembl("Homo sapiens", metadata = TRUE)
#' class(x)
#' names(x)
#' glimpse(x[["metadata"]])
ensembl <- function(
    organism,
    format = c("genes", "gene2symbol", "transcripts", "tx2gene"),
    genomeBuild = NULL,
    release = NULL,
    metadata = FALSE,
    return = c("GRanges", "DataFrame", "data.frame")
) {
    assert_is_a_string(organism)
    format <- match.arg(format)
    assertIsAStringOrNULL(genomeBuild)
    assertIsAnImplicitIntegerOrNULL(release)
    if (isAnImplicitInteger(release)) {
        # Note that ensembldb currently only supports >= 87
        assert_all_are_positive(release)
    }
    assert_is_a_bool(metadata)
    if (format %in% c("genes", "transcripts")) {
        return <- match.arg(return)
    }

    # Ensure `select()` isn't masked by ensembldb/AnnotationDbi
    userAttached <- .packages()

    # Catch UCSC genome build IDs ==============================================
    if (is_a_string(genomeBuild)) {
        map <- c(
            "hg19" = "GRCh37",
            "hg38" = "GRCh38",
            "mm10" = "GRCm38"
        )
        if (genomeBuild %in% names(map)) {
            map <- map[match(genomeBuild, names(map))]
            inform(paste("Remapping genome build", names(map), "to", map))
            genomeBuild <- as.character(map)
        }
    }

    # GRCh37 (release 75) ======================================================
    if (
        identical(tolower(organism), "homo sapiens") &&
        identical(tolower(genomeBuild), "grch37")
    ) {
        # TODO Replace with Homo_sapiens.GRCh37.75 annotation package
        release <- 75L
        abort("Request Homo_sapiens.GRCh37.75 support update")
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
        "Fetching Ensembl", format, "from AnnotationHub",
        packageVersion("AnnotationHub"),
        paste0("(", snapshotDate(ah), ")")
    ))

    # Use ensembldb annotations by default
    rdataclass <- "EnsDb"

    # Get the AnnotationHub dataset by identifier number
    ahdb <- query(
        ah,
        pattern = c("Ensembl", organism, release, genomeBuild, rdataclass),
        ignore.case = TRUE
    )
    mcols <- mcols(ahdb)

    if (nrow(mcols) > 1L) {
        if (!is.null(genomeBuild)) {
            mcols <- mcols %>%
                .[grep(genomeBuild, .[["genome"]], ignore.case = TRUE),
                    , drop = FALSE]
        }
        if (!is.null(release)) {
            mcols <- mcols %>%
                .[grep(release, .[["tags"]]), , drop = FALSE]
        }
        # Pick the latest release by AH identifier
        mcols <- tail(mcols, 1L)
    } else if (!nrow(mcols)) {
        # Abort on match failure
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
    id <- rownames(mcols)
    meta <- as.list(mcols)
    # Put the AnnotationHub ID first in the lists
    meta <- c("id" = id, meta)

    # ensembldb (EnsDb) ========================================================
    # This step will also output `txProgressBar()` on a fresh install. Using
    # `capture.output()` here again to suppress console output. Additionally, it
    # attaches ensembldb and other Bioconductor dependency packages, which will
    # mask some tidyverse functions (e.g. `select()`).
    invisible(capture.output(
        edb <- suppressMessages(ah[[id]])
    ))
    assert_is_all_of(edb, "EnsDb")

    inform(paste(
        paste0(id, ":"),
        organism(edb),
        meta[["genome"]],
        "Ensembl", ensemblVersion(edb),
        paste0("(", meta[["rdatadateadded"]], ")")
    ))

    if (format == "genes") {
        data <- genes(
            x = edb,
            order.by = "gene_id",
            return.type = return
        )
        if (return == "GRanges") {
            assert_has_names(data)
        } else {
            rownames(data) <- data[["gene_id"]]
        }
    } else if (format == "transcripts") {
        tx <- transcripts(
            x = edb,
            order.by = "tx_id",
            return.type = return
        )
        # Merge additional mcols of interest from gene annotations
        gene <- genes(
            x = edb,
            columns = c(
                "gene_id",
                "gene_name",
                "gene_biotype",
                "description",
                "entrezid"
            ),
            order.by = "gene_id",
            return.type = return
        )
        if (is(tx, "GRanges")) {
            txData <- mcols(tx)
            geneData <- mcols(gene)
        } else {
            txData <- tx
            geneData <- gene
        }
        mergeData <- merge(
            x = txData,
            y = geneData,
            by = "gene_id",
            all.x = TRUE,
            sort = FALSE
        ) %>%
            .[order(.[["tx_id"]]), , drop = FALSE]
        assert_are_identical(txData[["tx_id"]], mergeData[["tx_id"]])
        if (is(tx, "GRanges")) {
            mcols(tx) <- mergeData
        } else {
            tx <- mergeData
        }
        data <- tx
        if (return == "GRanges") {
            assert_has_names(data)
        } else {
            rownames(data) <- data[["tx_id"]]
        }
    } else if (format == "gene2symbol") {
        data <- genes(
            x = edb,
            columns = c("gene_id", "gene_name"),
            order.by = "gene_id",
            return.type = "data.frame"
        )
        rownames(data) <- data[["gene_id"]]
    } else if (format == "tx2gene") {
        data <- transcripts(
            x = edb,
            columns = c("tx_id", "gene_id"),
            order.by = "tx_id",
            return.type = "data.frame"
        )
        rownames(data) <- data[["tx_id"]]
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

    # Return ===================================================================
    # Sanitize columns
    data <- .sanitizeAnnotationCols(data)

    # Broad class definitions
    if (format %in% c("genes", "transcripts")) {
        data <- .addBroadClassCol(data)
    }

    # Double check that names are set correctly
    if (has_rows(data)) {
        assertHasRownames(data)
    } else {
        assert_has_names(data)
    }

    # Stash the AnnotationHub metadata inside a list, if desired
    if (isTRUE(metadata)) {
        data <- list("data" = data, "metadata" = meta)
    }

    data
}
