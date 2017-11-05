#' [Ensembl](http://www.ensembl.org/) Annotations
#'
#' Fetch annotations from AnnotationHub using the ensembldb package. This
#' function defaults to obtaining the latest annotations, unless the `release`
#' argument is set to a numeric version (e.g. 88).
#'
#' @rdname annotable
#' @name annotable
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams saveData
#'
#' @param object Object. Default usage is to provide Ensembl genome build as a
#'   character string.
#' @param format Desired table format, either `gene`, `tx2gene`, or
#'   `gene2symbol`.
#' @param release *Optional*. Ensembl release version. Defaults to the most
#'   current release available on AnnotationHub.
#'
#' @return [data.frame] with unique rows per gene or transcript.
#'
#' @seealso
#' - [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub).
#' - [ensembldb](https://doi.org/doi:10.18129/B9.bioc.ensembldb).
#'
#' @examples
#' annotable("Mus musculus") %>% str()
NULL



# Constructors ====
#' @importFrom AnnotationHub AnnotationHub getAnnotationHubOption query
#'   snapshotDate
#' @importFrom dplyr mutate rename
#' @importFrom ensembldb ensemblVersion genes transcripts
#' @importFrom magrittr set_rownames
#' @importFrom rlang .data is_string
#' @importFrom S4Vectors mcols
#' @importFrom utils tail
.annotable <- function(
    object,
    format = "gene",
    release,
    quiet = FALSE) {
    if (!is_string(object)) {
        stop("Object must be a string", call. = FALSE)
    }
    if (!format %in% c("gene", "gene2symbol", "tx2gene")) {
        stop("Unsupported format", call. = FALSE)
    }

    # Sanitize the release version
    if (missing(release)) {
        release <- NULL
    }
    if (is.numeric(release)) {
        if (release < 87L) {
            warning(paste(
                "AnnotationHub only supports Ensembl releases 87 and newer.",
                "Using current release instead."
            ), call. = FALSE)
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

    organism <- detectOrganism(object)

    # Download organism EnsDb package from AnnotationHub
    ah <- suppressMessages(AnnotationHub())

    if (!isTRUE(quiet)) {
        message(paste(
            "Loading Ensembl annotations from AnnotationHub",
            normalizePath(getAnnotationHubOption("CACHE")),
            snapshotDate(ah),
            sep = "\n"
        ))
    }

    # Get the AnnotationHub dataset by identifier number
    ahDb <- query(
        ah,
        pattern = c(organism, "EnsDb", releasePattern),
        ignore.case = TRUE)
    id <- ahDb %>%
        mcols() %>%
        rownames() %>%
        tail(n = 1L)
    edb <- suppressMessages(ah[[id]])

    if (!isTRUE(quiet)) {
        message(paste(
            "EnsDB", paste0(id, ":"),
            organism(edb),
            "Ensembl", ensemblVersion(edb)
        ))
    }

    if (format == "gene") {
        annotable <- genes(
            edb,
            return.type = "data.frame") %>%
            # Use `symbol` column instead
            mutate(gene_name = NULL) %>%
            rename(
                ensgene = .data[["gene_id"]],
                biotype = .data[["gene_biotype"]]) %>%
            prepareAnnotable()
    } else if (format == "gene2symbol") {
        annotable <- genes(
            edb,
            columns = c("gene_id", "symbol"),
            return.type = "data.frame") %>%
            rename(ensgene = .data[["gene_id"]]) %>%
            # Ensure unique symbols (e.g. human, mouse)
            mutate(symbol = make.unique(.data[["symbol"]])) %>%
            set_rownames(.[["ensgene"]])
    } else if (format == "tx2gene") {
        annotable <- transcripts(
            edb,
            columns = c("tx_id", "gene_id"),
            return.type = "data.frame") %>%
            rename(
                enstxp = .data[["tx_id"]],
                ensgene = .data[["gene_id"]]) %>%
            set_rownames(.[["enstxp"]])
    }
    annotable
}



# Methods ====
#' @rdname annotable
#' @export
setMethod(
    "annotable",
    signature("character"),
    .annotable)
