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
#'   `gene2symbol` output.
#' @param broadClass Include broad class definitions, based on `biotype` and/or
#'   gene `symbol` annotations. Only applies to `genes` and `transcripts`
#'   output.
#' @param sanitizeColnames Improve column names and drop duplicated columns:
#'   - Rename `geneID` to `ensgene`.
#'   - Rename `txID` to `enstxp`.
#'   - Rename `geneBiotype` or `txBiotype` to simply `biotype`.
#'   - Rename `entrezid` to `entrez`.
#' @param return Class of the returned object. Can be "`data.frame`",
#'   "`DataFrame`" or "`GRanges`. See `help("genes", "ensembldb")` for
#'   additional information.
#' @param metadata Include the AnnotationHub metadata inside a list. Useful
#'   for documenting where the annotations were sourced from inside
#'   AnnotationHub.
#'
#' @return `GRanges`, `data.frame`, or `DataFrame`.
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
    format = "genes",
    genomeBuild = NULL,
    release = NULL,
    uniqueSymbol = FALSE,
    broadClass = TRUE,
    sanitizeColnames = TRUE,
    return = "GRanges",
    metadata = FALSE) {
    assert_is_a_string(organism)
    assert_is_a_string(format)
    assert_is_subset(
        x = format,
        y = c("genes", "transcripts", "gene2symbol", "tx2gene"))
    assertIsAStringOrNULL(genomeBuild)
    assertIsAnImplicitIntegerOrNULL(release)
    if (isAnImplicitInteger(release)) {
        # AnnotableHub only supports releases 87 and above
        assert_all_are_greater_than_or_equal_to(release, 87L)
    }
    assert_is_a_bool(uniqueSymbol)
    assert_is_a_bool(broadClass)
    assert_is_a_bool(sanitizeColnames)
    .assertFormalEnsembldbReturn(return)
    assert_is_a_bool(metadata)

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
            genomeBuild <- as.character(map)
        }
    }

    # GRCh37 legacy support ====================================================
    if (
        identical(tolower(organism), "homo sapiens") &&
        identical(tolower(genomeBuild), "grch37")
    ) {
        if (format == "genes") {
            data <- load(system.file(
                "extdata/grch37.rda", package = "basejump"))
            data <- get(data, inherits = FALSE)
            return(data)
        } else if (format == "gene2symbol") {
            data <- load(system.file(
                "extdata/grch37.rda", package = "basejump"))
            data <- get(data, inherits = FALSE)
            data <- data[, c("ensgene", "symbol")]
            return(data)
        } else if (format == "tx2gene") {
            data <- load(system.file(
                "extdata/grch37Tx2gene.rda", package = "basejump"))
            data <- get(data, inherits = FALSE)
            return(data)
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

    # Get the AnnotationHub dataset by identifier number
    ahDb <- query(
        ah,
        pattern = c("EnsDb", organism, release, genomeBuild),
        ignore.case = TRUE)
    mcols <- mcols(ahDb)
    if (!is.null(genomeBuild)) {
        mcols <- mcols %>%
            .[grep(genomeBuild, .[["genome"]], ignore.case = TRUE), ,
              drop = FALSE]
    }
    if (!is.null(release)) {
        mcols <- mcols %>%
            .[grep(release, .[["tags"]]), , drop = FALSE]
    }
    # Abort on match failure
    if (!nrow(mcols)) {
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

    # Always pick the latest release by AH identifier
    match <- tail(mcols, 1L)
    id <- rownames(match)
    meta <- as.list(match)
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
                "entrezid"),
            return.type = return)
        idCol <- "gene_id"
    } else if (format == "transcripts") {
        data <- transcripts(
            edb,
            columns = c(
                "tx_id",  # `tx_name` is duplicate
                "tx_biotype",
                "tx_cds_seq_start",
                "tx_cds_seq_end",
                "tx_support_level",
                "gene_id"),
            return.type = return)
        idCol <- "tx_id"
    } else if (format == "gene2symbol") {
        data <- genes(
            edb,
            columns = c("gene_id", "symbol"),
            return.type = return)
        idCol <- "gene_id"
    } else if (format == "tx2gene") {
        data <- transcripts(
            edb,
            columns = c("tx_id", "gene_id"),
            return.type = return)
        idCol <- "tx_id"
    }

    # Convert column names into desired convention
    data <- camel(data)
    idCol <- camel(idCol)

    # Unique symbol mode
    if (format %in% c("genes", "gene2symbol") && isTRUE(uniqueSymbol)) {
        data <- .uniqueSymbol(data)
    }

    # Broad class definitions
    if (format %in% c("genes", "transcripts") && isTRUE(broadClass)) {
        data <- .addBroadClassCol(data)
    }

    # Sort by identifier
    if (is(data, "GRanges")) {
        data <- data %>%
            .[order(mcols(.)[[idCol]], start(.))]
    } else {
        # Add rownames
        rownames(data) <- data[[idCol]]
        data <- data %>%
            .[order(rownames(.)), , drop = FALSE]
    }

    # Sanitize columns
    if (isTRUE(sanitizeColnames)) {
        data <- .sanitizeAnnotationCols(data, format = format)
    }

    # Stash the AnnotationHub metadata inside a list, if desired
    if (isTRUE(metadata)) {
        data <- list(data = data, metadata = meta)
    }

    data
}



.sanitizeAnnotationCols <- function(object, format = "genes") {
    if (is(object, "GRanges")) {
        data <- mcols(object)
    } else {
        data <- object
    }

    # Rename the columns
    colnames(data) <- colnames(data) %>%
        camel() %>%
        gsub("^entrezid", "entrez", .) %>%
        gsub("^geneID$", "ensgene", .) %>%
        gsub("^(gene|tx)Biotype", "biotype", .) %>%
        gsub("^txID$", "enstxp", .)

    # Reorder priority columns for genes and transcripts
    if (format %in% c("genes", "transcripts")) {
        if (format == "genes") {
            priorityCols <- geneAnnotationCols
        } else if (format == "transcripts") {
            priorityCols <- transcriptAnnotationCols
        }
        # Add the `broadClass` column, if present
        if ("broadClass" %in% colnames(data)) {
            priorityCols <- c(priorityCols, "broadClass")
        }
        assert_is_subset(priorityCols, colnames(data))
        data <- data %>%
            .[, c(priorityCols, setdiff(colnames(.), priorityCols)),
              drop = FALSE]
    }

    if (is(object, "GRanges")) {
        mcols(object) <- data
    } else {
        object <- data
    }

    object
}
