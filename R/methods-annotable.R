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
#' @param release Ensembl release version. This function defaults to using the
#'   most current release available on AnnotationHub (`current`).
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
    release = "current",
    quiet = FALSE) {
    if (!is_string(object)) {
        stop("Object must be a string", call. = FALSE)
    }
    if (!format %in% c("gene", "gene2symbol", "tx2gene")) {
        stop("Unsupported format", call. = FALSE)
    }

    organism <- detectOrganism(object)

    # Download organism EnsDb package from AnnotationHub
    ah <- suppressMessages(
        AnnotationHub()
    )

    if (!isTRUE(quiet)) {
        message(paste(
            "Loading Ensembl annotations from AnnotationHub"
        ))
        message(normalizePath(getAnnotationHubOption("CACHE")))
        message(snapshotDate(ah))
    }

    # Check for unsupported Ensembl release request
    if (is.numeric(release) & release < 87L) {
        warning(paste(
            "ensembldb only supports Ensembl releases 87 and newer.",
            "Using current release instead."
        ), call. = FALSE)
        release <- "current"
    }

    if (release == "current") {
        ahDb <- query(
            ah,
            pattern = c(organism, "EnsDb"),
            ignore.case = TRUE)
        # Get the latest AnnotationHub dataset by identifier number
        id <- ahDb %>%
            mcols() %>%
            rownames() %>%
            tail(n = 1L)
        edb <- suppressMessages(ah[[id]])
    } else {
        ahDb <- query(
            ah,
            pattern = c(
                organism,
                "EnsDb",
                # Match against the version more specifically
                # (e.g. "v90")
                paste0("v", release)),
            ignore.case = TRUE)
        id <- ahDb %>%
            mcols() %>%
            rownames()
        edb <- suppressMessages(ahDb[[1L]])
    }

    if (!isTRUE(quiet)) {
        message(paste(
            "EnsDB", paste0(id, ":"),
            organism(edb),
            "Ensembl", ensemblVersion(edb)
        ))
    }

    if (format == "gene") {
        genes(
            edb,
            columns = c(
                "gene_id",
                "symbol",  # `gene_name` also works
                "description",
                "gene_biotype"),
            return.type = "data.frame") %>%
            rename(
                ensgene = .data[["gene_id"]],
                biotype = .data[["gene_biotype"]]) %>%
            prepareAnnotable()
    } else if (format == "gene2symbol") {
        genes(
            edb,
            columns = c("gene_id", "symbol"),
            return.type = "data.frame") %>%
            rename(ensgene = .data[["gene_id"]]) %>%
            # Ensure unique symbols (e.g. human, mouse)
            mutate(symbol = make.unique(.data[["symbol"]])) %>%
            set_rownames(.[["ensgene"]])
    } else if (format == "tx2gene") {
        transcripts(
            edb,
            columns = c("tx_id", "gene_id"),
            return.type = "data.frame") %>%
            rename(
                enstxp = .data[["tx_id"]],
                ensgene = .data[["gene_id"]]) %>%
            set_rownames(.[["enstxp"]])
    }
}



# Methods ====
#' @rdname annotable
#' @export
setMethod(
    "annotable",
    signature("character"),
    .annotable)
