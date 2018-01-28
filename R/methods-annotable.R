#' Ensembl Annotations
#'
#' Quickly obtain gene annotations from [Ensembl](http://www.ensembl.org/).
#'
#' @section Using AnnotationHub:
#' The default recommended approach is to simply specify the desired organism,
#' using the full latin name. For example, we can obtain human annotations with
#' `Homo sapiens` and mouse annotations with `Mus musculus`. Under the hood, the
#' function fetches these annotations from AnnotationHub using the ensembldb
#' package. AnnotationHub supports versioned Ensembl releases, back to version
#' 87.
#'
#' @section Using Pre-Compiled Annotables:
#' Generally we' don't recommend this approach. However, pass-in of a tibble
#' generated using [biomaRt](https://doi.org/doi:10.18129/B9.bioc.biomaRt) in
#' the [annotables](https://github.com/stephenturner/annotables) data package is
#' useful for adding support for the outdated (but still frequently used)
#' GRCh37/hg19 human genome.
#'
#' @section Broad Class Definitions:
#' A `broadClass` column is added, which generalizes the gene types into a
#' smaller number of semantically-meaningful groups:
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
#' @rdname annotable
#' @name annotable
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams saveData
#'
#' @param object Object. Default recommended usage is to provide full latin
#'   organism name as a string. Also supports input of pre-built tibbles from
#'   the annotables data package.
#' @param format Desired table format, either `gene`, `tx2gene`, or
#'   `gene2symbol`.
#' @param genomeBuild *Optional*. Check to see if a specific genome build is
#'   supported.
#' @param release *Optional*. Ensembl release version. Defaults to the most
#'   current release available on AnnotationHub.
#' @param uniqueSymbol Make gene symbols unique.
#'
#' @return [data.frame] with unique rows per gene or transcript.
#'
#' @seealso
#' - [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub).
#' - [ensembldb](https://doi.org/doi:10.18129/B9.bioc.ensembldb).
#'
#' @examples
#' annotable("Homo sapiens") %>% glimpse()
#'
#' # Legacy GRCh37/hg19 genome build support
#' annotable("Homo sapiens", genomeBuild = "GRCh37") %>% glimpse()
#'
#' # Convert pre-compiled annotables tibble (advanced)
#' \dontrun{
#' annotable(annotables::grch38) %>% glimpse()
#' }
NULL



# Constructors =================================================================
#' @importFrom AnnotationHub AnnotationHub getAnnotationHubOption query
#'   snapshotDate
#' @importFrom BiocGenerics organism
#' @importFrom dplyr mutate rename
#' @importFrom ensembldb ensemblVersion genes transcripts
#' @importFrom magrittr set_rownames
#' @importFrom S4Vectors mcols
#' @importFrom utils capture.output find tail
.annotable <- function(
    object,
    format = "gene",
    genomeBuild = NULL,
    release = NULL,
    uniqueSymbol = FALSE,
    quiet = FALSE) {
    # object (organism)
    if (!is_string(object)) {
        abort("Object must be a string")
    } else if (is.null(object)) {
        return(NULL)
    }
    # format
    if (!format %in% c("gene", "gene2symbol", "tx2gene")) {
        abort("Unsupported format")
    }

    if (!is.null(genomeBuild)) {
        # GRCh37/hg19 support
        if (object == "Homo sapiens" &
            grepl(
                pattern = paste("GRCh37", "hg19", sep = "|"),
                x = genomeBuild,
                ignore.case = TRUE)
        ) {
            if (format == "gene") {
                return(basejump::grch37)
            } else if (format == "gene2symbol") {
                return(basejump::grch37[, c("ensgene", "symbol")])
            } else if (format == "tx2gene") {
                return(basejump::grch37Tx2gene)
            }
        }
    }

    # Sanitize the release version
    if (is.numeric(release)) {
        if (release < 87L) {
            warn(paste(
                "AnnotationHub only supports Ensembl releases 87 and newer.",
                "Using current release instead."
            ))
            release <- NULL
        }
    } else {
        # Legacy code support for "current" (changed in 0.1.1)
        release <- NULL
    }
    if (is.null(release)) {
        releasePattern <- NULL
    } else {
        releasePattern <- paste0("v", release)
    }

    # Initialize AnnotationHub. On a fresh install this will print a
    # txProgressBar to the console. We're using `capture.output()` here
    # to suppress the console output, since it's not very informative and
    # can cluster R Markdown reports.
    invisible(capture.output(
        ah <- suppressMessages(AnnotationHub())
    ))

    if (!isTRUE(quiet)) {
        inform(paste(
            "Loading Ensembl annotations from AnnotationHub",
            snapshotDate(ah),
            sep = "\n"
        ))
    }

    # Get the AnnotationHub dataset by identifier number
    ahDb <- query(
        ah,
        pattern = c(object, "EnsDb", releasePattern),
        ignore.case = TRUE)
    # Get the latest build version
    id <- ahDb %>%
        mcols() %>%
        rownames() %>%
        tail(n = 1L)

    # Early return `NULL` with warning on organism failure
    if (!length(id)) {
        warn(paste(
            object, "is not supported in AnnotationHub"
        ))
        return(NULL)
    }

    # This step will also output `txProgressBar()` on a fresh install. Using
    # `capture.output()` here again to suppress console output.
    invisible(capture.output(
        edb <- suppressMessages(ah[[id]])
    ))

    # Ensure `dplyr::select()` isn't masked by AnnotationHub/ensembldb
    packages <- find("select")
    # Note that this will return a character vector for multiple matches,
    # and we want to exclude dplyr from the detach loop
    packages <- setdiff(packages, "package:dplyr")
    if (length(packages)) {
        lapply(packages, function(name) {
            suppressWarnings(detach(
                name = name,
                character.only = TRUE,
                unload = TRUE,
                force = TRUE
            ))
        }) %>%
            invisible()
    }

    if (!isTRUE(quiet)) {
        inform(paste(
            "EnsDB", paste0(id, ":"),
            organism(edb),
            "Ensembl", ensemblVersion(edb)
        ))
    }

    if (format == "gene") {
        data <- genes(edb, return.type = "data.frame") %>%
            rename(
                ensgene = .data[["gene_id"]],
                biotype = .data[["gene_biotype"]]) %>%
            # Use `symbol` column instead of duplicate `gene_name`
            mutate(gene_name = NULL) %>%
            # Ensure rows are sorted by gene ID
            arrange(!!sym("ensgene"))
        if (isTRUE(uniqueSymbol)) {
            # Ensure unique symbols (e.g. human, mouse)
            data[["symbol"]] <- make.unique(data[["symbol"]], sep = ".")
        }
        data <- .prepareAnnotable(data)
    } else if (format == "gene2symbol") {
        data <- genes(
            edb,
            columns = c("gene_id", "symbol"),
            return.type = "data.frame") %>%
            rename(ensgene = .data[["gene_id"]]) %>%
            # Ensure rows are sorted by gene ID
            arrange(!!sym("ensgene"))
        if (isTRUE(uniqueSymbol)) {
            # Ensure unique symbols (e.g. human, mouse)
            data[["symbol"]] <- make.unique(data[["symbol"]], sep = ".")
        }
        rownames(data) <- data[["ensgene"]]
    } else if (format == "tx2gene") {
        data <- transcripts(
            edb,
            columns = c("tx_id", "gene_id"),
            return.type = "data.frame") %>%
            rename(
                enstxp = .data[["tx_id"]],
                ensgene = .data[["gene_id"]]) %>%
            arrange(!!sym("enstxp")) %>%
            set_rownames(.[["enstxp"]])
    }

    data
}



#' Define Broad Class
#'
#' @author Broad class definitions by Rory Kirchner
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr case_when mutate
#'
#' @return [data.frame] with `broadClass` column.
.defineBroadClass <- function(object) {
    requiredCols <- c("biotype", "symbol")
    if (!all(requiredCols %in% colnames(object))) {
        abort(paste(
            "Required columns:", toString(object)
        ))
    }
    mutate(
        object,
        broadClass = case_when(
            grepl(
                x = .data[["symbol"]],
                # Hsapiens: `MT-`,
                # Mmusculus: `mt-`
                # Dmelanogaster: `mt:`
                pattern = "^mt[\\:\\-]",
                ignore.case = TRUE
            ) ~ "mito",
            .data[["biotype"]] == "protein_coding" ~ "coding",
            .data[["biotype"]] %in% c(
                "known_ncrna",
                "lincRNA",
                "non_coding"
            ) ~ "noncoding",
            grepl(
                pattern = "pseudo",
                x = .data[["biotype"]]
            ) ~ "pseudo",
            .data[["biotype"]] %in% c(
                "miRNA",
                "misc_RNA",
                "ribozyme",
                "rRNA",
                "scaRNA",
                "scRNA",
                "snoRNA",
                "snRNA",
                "sRNA"
            ) ~ "small",
            .data[["biotype"]] %in% c(
                "non_stop_decay",
                "nonsense_mediated_decay"
            ) ~ "decaying",
            grepl(
                pattern = "^ig_",
                x = .data[["biotype"]],
                ignore.case = TRUE
            ) ~ "ig",
            grepl(
                pattern = "^tr_",
                x = .data[["biotype"]],
                ignore.case = TRUE
            ) ~ "tcr",
            TRUE ~ "other")
    )
}



#' Prepare Annotable
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr distinct everything group_by left_join mutate rename select
#'   summarize_all ungroup
#' @importFrom magrittr set_rownames
#' @importFrom S4Vectors aggregate
#' @importFrom stats formula
#'
#' @inheritParams AllGenerics
#'
#' @return [data.frame].
.prepareAnnotable <- function(object) {
    # Check for required columns
    requiredCols <- c("ensgene", "symbol", "description", "biotype")
    if (!all(requiredCols %in% colnames(object))) {
        abort(paste(
            "Required columns:", toString(requiredCols)
        ))
    }
    # Check for Entrez identifier column and nest into a list, if necessary
    entrezCol <- colnames(object) %>%
        .[grepl(x = ., pattern = "entrez")]
    if (length(entrezCol) & entrezCol != "entrez") {
        # Standard to `entrez`. ensembldb outputs as `entrezid`.
        object <- rename(object, entrez = !!sym(entrezCol))
        rm(entrezCol)
    }
    # Check for annotable that needs the Entrez IDs nested (e.g. biomaRt output)
    if (!is.null(object[["entrez"]]) &
        !is.list(object[["entrez"]]) &
        any(duplicated(object[["ensgene"]]))) {
        # Alternatively can use `tidyr::nest()` approach here instead but
        # the output structure won't be consistent with the ensembl return.
        entrez <- aggregate(
            formula = formula("entrez~ensgene"),
            data = object,
            FUN = list
        )
        # Now drop the `entrez` column and add the aggregated list version
        object <- object %>%
            mutate(entrez = NULL) %>%
            distinct() %>%
            left_join(entrez, by = "ensgene")
    }
    # Collapse rows by Ensembl ID, if necessary
    if (any(duplicated(object[["ensgene"]]))) {
        object <- object %>%
            group_by(!!!syms(requiredCols)) %>%
            summarize_all(funs(
                collapseToString(object = ., unique = TRUE, sort = TRUE)
            )) %>%
            ungroup()
    }
    object %>%
        camel(strict = FALSE) %>%
        fixNA() %>%
        .defineBroadClass() %>%
        select(c(requiredCols, "broadClass"), everything()) %>%
        arrange(!!sym("ensgene")) %>%
        as.data.frame() %>%
        set_rownames(.[["ensgene"]])
}



# Methods ======================================================================
#' @rdname annotable
#' @export
setMethod(
    "annotable",
    signature("character"),
    .annotable)



#' @rdname annotable
#' @export
setMethod(
    "annotable",
    signature("data.frame"),
    .prepareAnnotable)
