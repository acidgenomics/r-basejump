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
#'
#' @return [data.frame] with unique rows per gene or transcript.
#'
#' @seealso -
#' [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub). -
#' [ensembldb](https://doi.org/doi:10.18129/B9.bioc.ensembldb).
#'
#' @examples
#' annotable("Mus musculus") %>% str()
#'
#' # Alternate approach, with pre-compiled tibble
#' \dontrun{
#' annotable(annotables::grch37)
#' }
#'
#' # Unsupported organism
#' annotable("XXX")
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
    genomeBuild = NULL,
    release = NULL,
    quiet = FALSE) {
    if (!is_string(object)) {
        stop("Object must be a string", call. = FALSE)
    }
    if (!format %in% c("gene", "gene2symbol", "tx2gene")) {
        stop("Unsupported format", call. = FALSE)
    }

    organism <- detectOrganism(object)
    if (is.null(organism)) {
        return(NULL)
    }

    if (!is.null(genomeBuild)) {
        .checkAnnotableBuildSupport(genomeBuild)
    }

    # Sanitize the release version
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

    # Initialize AnnotationHub. On a fresh install this will print a
    # txProgressBar to the console. We're using `capture.output()` here
    # to suppress the console output, since it's not very informative and
    # can cluster R Markdown reports.
    invisible(capture.output(
        ah <- suppressMessages(AnnotationHub())
    ))

    if (!isTRUE(quiet)) {
        message(paste(
            "Loading Ensembl annotations from AnnotationHub",
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

    # AnnotationHub will attach ensembldb at this step and mask dplyr!
    if ("ensembldb" %in% .packages()) {
        ensembldbUserAttached <- TRUE
    } else {
        ensembldbUserAttached <- FALSE
    }

    # This step will also output a txProgressBar on a fresh install. Using
    # `capture.output()` here again to suppress console output.
    invisible(capture.output(
        edb <- suppressMessages(ah[[id]])
    ))

    if ("ensembldb" %in% .packages() &
        !isTRUE(ensembldbUserAttached)) {
        suppressWarnings(
            detach("package:ensembldb", unload = TRUE, force = TRUE)
        )
    }

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
            .prepareAnnotable()
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



.checkAnnotableBuildSupport <- function(genomeBuild) {
    # Require prebuilt grch37 annotable for GRCh37/hg19
    if (grepl(x = genomeBuild,
              pattern = paste("GRCh37", "hg19", sep = "|"),
              ignore.case = TRUE)) {
        stop(paste(
            "GRCh37/hg19 detected.",
            "'annotable' argument must be set to 'annotables::grch37'."
        ), call. = FALSE)
    }
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
        stop(paste(
            "Required columns:", toString(object)
        ), call. = FALSE)
    }
    mutate(
        object,
        # Ensure unique symbols (e.g. human, mouse)
        symbol = make.unique(.data[["symbol"]]),
        # Define the broad class
        broadClass = case_when(
            grepl(
                x = .data[["symbol"]],
                # Hsapiens: `MT-`,
                # Mmusculus: `mt-`
                # Dmelanogaster: `mt:`
                pattern = "^mt[\\:\\-]",
                ignore.case = TRUE) ~ "mito",
            .data[["biotype"]] == "protein_coding" ~ "coding",
            .data[["biotype"]] %in%
                c("known_ncrna",
                  "lincRNA",
                  "non_coding") ~ "noncoding",
            grepl(
                x = .data[["biotype"]],
                pattern = "pseudo") ~ "pseudo",
            .data[["biotype"]] %in%
                c("miRNA",
                  "misc_RNA",
                  "ribozyme",
                  "rRNA",
                  "scaRNA",
                  "scRNA",
                  "snoRNA",
                  "snRNA",
                  "sRNA") ~ "small",
            .data[["biotype"]] %in%
                c("non_stop_decay",
                  "nonsense_mediated_decay") ~ "decaying",
            grepl(
                x = .data[["biotype"]],
                pattern = "^ig_",
                ignore.case = TRUE) ~ "ig",
            grepl(
                x = .data[["biotype"]],
                pattern = "^tr_",
                ignore.case = TRUE) ~ "tcr",
            TRUE ~ "other")
    )
}



#' Prepare Annotable
#'

#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr distinct group_by summarize_all ungroup
#' @importFrom magrittr set_rownames
#' @importFrom rlang !! !!! sym syms
#'
#' @inheritParams AllGenerics
#'
#' @return [data.frame].
.prepareAnnotable <- function(object) {
    # Check for required columns
    requiredCols <- c("ensgene", "symbol", "description", "biotype")
    if (!all(requiredCols %in% colnames(object))) {
        stop(paste(
            "Required columns:", toString(requiredCols)
        ), call. = FALSE)
    }
    # Drop the entrez identifiers, if detected
    if (any(grepl(x = colnames(object), pattern = "entrez"))) {
        object <- object %>%
            .[, !grepl(x = colnames(.), pattern = "entrez")] %>%
            distinct()
    }
    # Collapse remaining nondistinct columns, if necessary
    if (anyDuplicated(object[["ensgene"]])) {
        object <- object %>%
            group_by(!!!syms(requiredCols)) %>%
            summarize_all(funs(
                collapseToString(object = ., unique = TRUE, sort = TRUE)
            )) %>%
            ungroup()
    }
    object %>%
        camel(strict = FALSE) %>%
        # Improve handling of `NA` uniques here
        fixNA() %>%
        .defineBroadClass() %>%
        as.data.frame() %>%
        select(c(requiredCols, "broadClass"), everything()) %>%
        arrange(!!sym("ensgene")) %>%
        set_rownames(.[["ensgene"]])
}



# Methods ====
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
