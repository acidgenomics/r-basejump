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
#'  unnecessary and can be left `NULL`. If a count matrix starts with a
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
.g2svec <- function(
    object,
    organism = NULL,
    release = NULL,
    quiet = FALSE) {
    if (is_string(object)) {
        stop("gene2symbol conversion requires > 1 identifier",
             call. = FALSE)
    }
    if (any(is.na(object))) {
        stop("NA identifier detected", call. = FALSE)
    }
    if (any(object == "")) {
        stop("Empty string identifier detected", call. = FALSE)
    }
    if (any(duplicated(object))) {
        warning("Duplicate gene identifiers detected", call. = FALSE)
    }

    # Detect organism
    if (is.null(organism)) {
        organism <- detectOrganism(object[[1L]])
    } else {
        organism <- detectOrganism(organism)
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



# Pass arguments to `.g2svec()`
.g2sdim <- function(
    object,
    organism = NULL,
    release = NULL,
    quiet = FALSE) {
    rownames(object) <- rownames(object) %>%
        .g2svec(organism = organism,
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
    .g2svec)



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("data.frame"),
    .g2sdim)



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("DataFrame"),
    .g2sdim)



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("dgCMatrix"),
    .g2sdim)



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("dgTMatrix"),
    .g2sdim)



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("matrix"),
    .g2sdim)
