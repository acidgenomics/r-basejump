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
#' @param uniqueSymbol Make gene symbols unique. Only applies to `genes` and
#'   `gene2symbol` output. *Generally this is not recommended.*
#' @param metadata Include the AnnotationHub metadata inside a list. Useful
#'   for documenting where the annotations were sourced from inside
#'   AnnotationHub.
#' @param return Class of the returned object. Can be "`GRanges`" (default),
#'   "`DataFrame`", or "`data.frame`". See `help("genes", "ensembldb")` for
#'   additional information.
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
    format = c("genes", "transcripts", "gene2symbol", "tx2gene"),
    genomeBuild = NULL,
    release = NULL,
    uniqueSymbol = FALSE,
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
    assert_is_a_bool(uniqueSymbol)
    assert_is_a_bool(metadata)
    return <- match.arg(return)

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

    # GRCh37 support
    if (
        identical(tolower(organism), "homo sapiens") &&
        identical(tolower(genomeBuild), "grch37")
    ) {
        rdataclass <- "GRanges"
        release <- 75L
    }

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

    # ensembldb ================================================================
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
        data <- genes(
            edb,
            columns = c(
                "gene_id",
                "symbol",  # `gene_name` is duplicate
                "description",
                "gene_biotype",
                "gene_seq_start",
                "gene_seq_end",
                "seq_name",
                "seq_strand",
                "seq_coord_system",
                "entrezid"
            ),
            order.by = "gene_id",
            return.type = return
        )
    } else if (format == "gene2symbol") {
        data <- genes(
            edb,
            columns = c("gene_id", "symbol"),
            order.by = "gene_id",
            return.type = return
        )
    } else if (format == "transcripts") {
        data <- transcripts(
            edb,
            columns = c(
                "tx_id",  # `tx_name` is duplicate
                "tx_biotype",
                "tx_cds_seq_start",
                "tx_cds_seq_end",
                "tx_support_level",
                "gene_id"
            ),
            order.by = "tx_id",
            return.type = return
        )
    } else if (format == "tx2gene") {
        data <- transcripts(
            edb,
            columns = c("tx_id", "gene_id"),
            order.by = "tx_id",
            return.type = return
        )
    }

    # Convert column names into desired convention
    data <- camel(data)

    # Broad class definitions
    if (format %in% c("genes", "transcripts")) {
        data <- .addBroadClassCol(data)
    }

    # Sanitize columns
    data <- .sanitizeAnnotationCols(data, format = format)

    # Unique symbol mode
    if (format %in% c("genes", "gene2symbol") && isTRUE(uniqueSymbol)) {
        data <- .uniqueSymbol(data)
    }

    # Stash the AnnotationHub metadata inside a list, if desired
    if (isTRUE(metadata)) {
        data <- list(data = data, metadata = meta)
    }

    data
}
