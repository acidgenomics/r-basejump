#' Convert Ensembl Transcripts to Genes
#'
#' @rdname convertTranscriptsToGenes
#' @name convertTranscriptsToGenes
#' @family Gene Annotation Utilities
#'
#' @inheritParams general
#' @inheritParams convertGenesToSymbols
#'
#' @param tx2gene *Optional.* Transcript-to-gene mappings. If `NULL`, will
#'   attempt to download from Ensembl using the desired `organism`,
#'   `genomeBuild`, and `release` arguments.
#'
#' @return Same class as object.
#'
#' @examples
#' # character
#' c("ENSMUST00000000001", "ENSMUST00000000003", "ENSMUST00000114041") %>%
#'     convertTranscriptsToGenes()
#'
#' # matrix
#' matrix(
#'     data = seq(1L:6L),
#'     byrow = TRUE,
#'     nrow = 3L,
#'     ncol = 2L,
#'     dimnames = list(
#'         c("ENSMUST00000000001", "ENSMUST00000000003", "ENSMUST00000114041"),
#'         c("sample1", "sample2")
#'     )
#' ) %>%
#'     convertTranscriptsToGenes()
NULL



# Constructors =================================================================
.convertTranscriptsToGenes <- function(
    object,
    tx2gene = NULL,
    organism = NULL,
    genomeBuild = NULL,
    release = NULL,
    quiet = FALSE) {
    # Passthrough: genomeBuild, release, quiet
    assert_is_character(object)
    assert_all_are_non_missing_nor_empty_character(object)
    assert_has_no_duplicates(object)
    assert_is_any_of(tx2gene, c("data.frame", "NULL"))
    assert_is_a_string_or_null(organism)
    assert_is_numeric_scalar_or_null(release)
    assert_is_a_bool(quiet)

    # If no tx2gene is provided, fall back to using Ensembl annotations
    if (!is.data.frame(tx2gene)) {
        # Generate tx2gene from Ensembl
        if (!isTRUE(quiet)) {
            inform("Obtaining transcript-to-gene mappings from Ensembl")
        }
        if (is.null(organism)) {
            organism <- detectOrganism(object, unique = TRUE)
        } else if (is_a_string(organism)) {
            organism <- detectOrganism(organism)
        }
        assert_is_a_string(organism)
        tx2gene <- tx2gene(
            object = organism,
            genomeBuild = genomeBuild,
            release = release,
            quiet = quiet)
    } else {
        assert_is_tx2gene(tx2gene)
    }

    tx2gene <- tx2gene %>%
        .[object, , drop = FALSE] %>%
        .[!is.na(.[["ensgene"]]), , drop = FALSE]

    gene <- tx2gene[["ensgene"]]
    names(gene) <- tx2gene[["enstxp"]]

    if (!all(object %in% names(gene))) {
        abort(paste(
            "Unmatched transcripts present.",
            "Try using a GFF file instead."
        ))
    }

    assert_is_character(gene)
    assert_has_names(gene)
    gene[object]
}



.convertTranscriptsToGenes.dim <- function(  # nolint
    object,
    tx2gene = NULL,
    organism = NULL,
    genomeBuild = NULL,
    release = NULL,
    quiet = FALSE) {
    # Passthrough: tx2gene, organism, genomeBuild, release, quiet
    rownames(object) <- .convertTranscriptsToGenes(
        object = rownames(object),
        tx2gene = tx2gene,
        organism = organism,
        genomeBuild = genomeBuild,
        release = release,
        quiet = quiet)
    object
}



# Methods ======================================================================
#' @rdname convertTranscriptsToGenes
setMethod(
    "convertTranscriptsToGenes",
    signature("character"),
    .convertTranscriptsToGenes)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    "convertTranscriptsToGenes",
    signature("data.frame"),
    .convertTranscriptsToGenes.dim)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    "convertTranscriptsToGenes",
    signature("DataFrame"),
    .convertTranscriptsToGenes.dim)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    "convertTranscriptsToGenes",
    signature("dgCMatrix"),
    .convertTranscriptsToGenes.dim)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    "convertTranscriptsToGenes",
    signature("dgTMatrix"),
    .convertTranscriptsToGenes.dim)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    "convertTranscriptsToGenes",
    signature("matrix"),
    .convertTranscriptsToGenes.dim)
