#' Convert Ensembl Transcripts to Genes
#'
#' @rdname convertTranscriptsToGenes
#' @name convertTranscriptsToGenes
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams annotable
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
    release = NULL,
    quiet = FALSE) {
    # Prevent pass in of genomeBuild as primary object.
    # Improve this in a future update.
    if (any(is.na(object))) {
        abort("NA identifier detected")
    }
    if (any(object == "")) {
        abort("Empty string identifier detected")
    }
    organism <- detectOrganism(object[[1L]])
    tx2gene <- annotable(
        organism,
        format = "tx2gene",
        release = release,
        quiet = quiet) %>%
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
    gene[object]
}



.convertTranscriptsToGenes.dim <- function(  # nolint
    object,
    release = NULL,
    quiet = FALSE) {
    rownames(object) <- rownames(object) %>%
        .convertTranscriptsToGenes(
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
