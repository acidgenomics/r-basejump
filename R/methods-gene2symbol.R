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
#' vec <- c("ENSMUSG00000000001", "ENSMUSG00000000003")
#' gene2symbol(vec)
#'
#' # matrix
#' mat <- matrix(
#'     data = seq(1L:4L),
#'     byrow = TRUE,
#'     nrow = 2L,
#'     ncol = 2L,
#'     dimnames = list(
#'         c("ENSMUSG00000000001",
#'           "ENSMUSG00000000003"),
#'         c("sample1",
#'           "sample2")
#'     )
#' )
#' gene2symbol(mat)
NULL



# Constructors =================================================================
.gene2symbol <- function(
    object,
    organism,
    release,
    quiet = FALSE) {
    if (missing(release)) release <- NULL

    if (is_string(object)) {
        abort("gene2symbol conversion requires > 1 identifier")
    } else if (any(is.na(object))) {
        abort("NA identifier detected")
    } else if (any(object == "")) {
        abort("Empty string identifier detected")
    } else if (any(duplicated(object))) {
        warn("Duplicate gene identifiers detected")
    }

    # Detect organism
    if (missing(organism)) {
        organism <- detectOrganism(object[[1L]])
    } else {
        organism <- detectOrganism(organism)
    }
    if (is.null(organism)) {
        warn("Returning unmodified gene identifiers")
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
        warn(paste(
            "Failed to match all gene IDs to symbols:",
            toString(nomatch)
            ))
        symbol <- c(symbol, nomatch)
    }

    symbol[object]
}



.gene2symbolDim <- function(
    object,
    organism,
    release,
    quiet = FALSE) {
    x <- rownames(object)
    x <- .gene2symbol(
        object = x,
        organism = organism,
        release = release,
        quiet = quiet)
    rownames(object) <- x
    object
}



# Methods ======================================================================
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
