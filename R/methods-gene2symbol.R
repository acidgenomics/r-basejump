#' Convert Ensembl Identifier to Gene Symbol
#'
#' @rdname gene2symbol
#' @name gene2symbol
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams annotable
#'
#' @param organism *Optional*. Organism name. Normally this argument is
#'  unnecessary and can be left unset. If a count matrix starts with a
#'  FASTA spike-in (e.g. "EGFP"), then automatic genome detection based on the
#'  first gene identifier will fail. In this case, the desired organism must be
#'  manually declared.
#'
#' @return Same class as original object.
#'
#' @seealso [detectOrganism()].
#'
#' @examples
#' # character
#' c("ENSMUSG00000000001",
#'   "ENSMUSG00000000003") %>%
#'   gene2symbol()
#'
#' # matrix
#' matrix(
#'     data = seq(1L:4L),
#'     byrow = TRUE,
#'     nrow = 2L,
#'     ncol = 2L,
#'     dimnames = list(c("ENSMUSG00000000001",
#'                       "ENSMUSG00000000003"),
#'                     c("sample1",
#'                       "sample2"))) %>%
#'     gene2symbol()
NULL



# Constructors ====
#' @importFrom rlang is_string
.gene2symbol <- function(
    object,
    organism,
    release,
    quiet = FALSE) {
    if (missing(release)) release <- NULL

    if (is_string(object)) {
        stop("gene2symbol conversion requires > 1 identifier", call. = FALSE)
    } else if (any(is.na(object))) {
        stop("NA identifier detected", call. = FALSE)
    } else if (any(object == "")) {
        stop("Empty string identifier detected", call. = FALSE)
    } else if (any(duplicated(object))) {
        warning("Duplicate gene identifiers detected", call. = FALSE)
    }

    # Detect organism
    if (missing(organism)) {
        organism <- detectOrganism(object[[1L]])
    } else {
        organism <- detectOrganism(organism)
    }
    if (is.null(organism)) {
        warning("Returning unmodified gene identifiers", call. = FALSE)
        names(object) <- object
        return(object)
    }

    g2s <- annotable(
        organism,
        format = "gene2symbol",
        release = release,
        quiet = quiet) %>%
        .[object, , drop = FALSE] %>%
        .[!is.na(.[["symbol"]]), , drop = FALSE]

    symbol <- g2s[["symbol"]]
    names(symbol) <- g2s[["ensgene"]]
    if (!all(object %in% names(symbol))) {
        nomatch <- setdiff(object, rownames(g2s))
        names(nomatch) <- nomatch
        warning(paste(
            "Failed to match all gene IDs to symbols:",
            toString(nomatch)
            ), call. = FALSE)
        symbol <- c(symbol, nomatch)
    }

    symbol[object]
}



# Pass arguments to `.gene2symbol()`
.gene2symbolDim <- function(
    object,
    organism = NULL,
    release = NULL,
    quiet = FALSE) {
    rownames(object) <- rownames(object) %>%
        .gene2symbol(organism = organism,
                release = release,
                quiet = quiet)
    object
}



# Methods ====
#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("character"),
    .gene2symbol)



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("data.frame"),
    .gene2symbolDim)



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("DataFrame"),
    .gene2symbolDim)



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("dgCMatrix"),
    .gene2symbolDim)



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("dgTMatrix"),
    .gene2symbolDim)



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("matrix"),
    .gene2symbolDim)
