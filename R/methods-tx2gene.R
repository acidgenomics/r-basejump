#' Convert Ensembl Transcript to Gene
#'
#' @rdname tx2gene
#' @name tx2gene
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams annotable
#'
#' @return Same class as object.
#' @export
#'
#' @examples
#' # character
#' c("ENSMUST00000000001",
#'   "ENSMUST00000000003",
#'   "ENSMUST00000114041") %>%
#'   tx2gene()
#'
#' # matrix
#' matrix(
#'     data = seq(1L:6L),
#'     byrow = TRUE,
#'     nrow = 3L,
#'     ncol = 2L,
#'     dimnames = list(c("ENSMUST00000000001",
#'                       "ENSMUST00000000003",
#'                       "ENSMUST00000114041"),
#'                     c("sample1",
#'                       "sample2"))) %>%
#'     tx2gene()
NULL



# Constructors =================================================================
.tx2gene <- function(
    object,
    release = NULL,
    quiet = FALSE) {
    # Prevent pass in of genomeBuild as primary object.
    # Improve this in a future update.
    if (is_string(object)) {
        abort("tx2gene conversion requires > 1 identifier")
    }
    if (any(is.na(object))) {
        abort("NA identifier detected")
    }
    if (any(object == "")) {
        abort("Empty string identifier detected")
    }
    organism <- detectOrganism(object[[1L]])
    t2g <- annotable(
        organism,
        format = "tx2gene",
        release = release,
        quiet = quiet) %>%
        .[object, ] %>%
        .[!is.na(.[["ensgene"]]), ]
    gene <- t2g[["ensgene"]]
    names(gene) <- t2g[["enstxp"]]
    if (!all(object %in% names(gene))) {
        abort(paste(
            "Unmatched transcripts present.",
            "Try using a GFF file instead."
        ))
    }
    gene[object]
}



.tx2geneDim <- function(
    object,
    release = NULL,
    quiet = FALSE) {
    rownames(object) <- rownames(object) %>%
        .tx2gene(
            release = release,
            quiet = quiet)
    object
}



# Methods ======================================================================
#' @rdname tx2gene
setMethod(
    "tx2gene",
    signature("character"),
    .tx2gene)



#' @rdname tx2gene
#' @export
setMethod(
    "tx2gene",
    signature("data.frame"),
    .tx2geneDim)



#' @rdname tx2gene
#' @export
setMethod(
    "tx2gene",
    signature("DataFrame"),
    .tx2geneDim)



#' @rdname tx2gene
#' @export
setMethod(
    "tx2gene",
    signature("dgCMatrix"),
    .tx2geneDim)



#' @rdname tx2gene
#' @export
setMethod(
    "tx2gene",
    signature("dgTMatrix"),
    .tx2geneDim)



#' @rdname tx2gene
#' @export
setMethod(
    "tx2gene",
    signature("matrix"),
    .tx2geneDim)
