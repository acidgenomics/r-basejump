#' Convert Gene Symbol to Ensembl Identifier
#'
#' @rdname symbol2gene
#' @name symbol2gene
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams annotable
#'
#' @param organism Organism name.
#'
#' @return Same class as original object.
#'
#' @seealso [detectOrganism()].
#'
#' @examples
#' # character
#' symbol2gene(c("Gnai3", "Pbsn"), organism = "Mus musculus")
#'
#' # matrix
#' mat <- matrix(
#'     data = seq(1L:4L),
#'     byrow = TRUE,
#'     nrow = 2L,
#'     ncol = 2L,
#'     dimnames = list(
#'         c("Gnai3", "Pbsn"),
#'         c("sample1", "sample2")
#'     )
#' )
#' symbol2gene(mat, organism = "Mus musculus")
NULL



# Constructors =================================================================
.symbol2gene <- function(
    object,
    organism,
    release,
    quiet = FALSE) {
    if (missing(release)) release <- NULL

    # Prevent pass in of organism as primary object.
    # Improve this in a future update.
    if (is_a_string(object)) {
        abort("symbol2gene conversion requires > 1 identifier")
    }
    if (any(is.na(object))) {
        abort("NA identifier detected")
    }
    if (any(object == "")) {
        abort("Empty string identifier detected")
    }
    if (any(duplicated(object))) {
        warn("Duplicate gene symbols detected")
    }

    # Detect organism
    organism <- detectOrganism(organism)

    # Get gene2symbol annotable
    g2s <- annotable(
        organism,
        format = "gene2symbol",
        release = release,
        quiet = quiet) %>%
        .[.[["symbol"]] %in% object, , drop = FALSE]

    ensgene <- g2s[["ensgene"]]
    names(ensgene) <- g2s[["symbol"]]
    if (!all(object %in% names(ensgene))) {
        nomatch <- setdiff(object, g2s[["symbol"]])
        names(nomatch) <- nomatch
        warn(paste(
            "Failed to match all gene symbols to IDs:",
            toString(nomatch)
        ))
        ensgene <- c(ensgene, nomatch)
    }

    ensgene[object]
}



.symbol2geneDim <- function(
    object,
    organism,
    release,
    quiet = FALSE) {
    x <- rownames(object)
    x <- .symbol2gene(
        object = x,
        organism = organism,
        release = release,
        quiet = quiet)
    rownames(object) <- x
    object
}



# Methods ======================================================================
#' @rdname symbol2gene
#' @export
setMethod(
    "symbol2gene",
    signature("character"),
    .symbol2gene)



#' @rdname symbol2gene
#' @export
setMethod(
    "symbol2gene",
    signature("data.frame"),
    .symbol2geneDim)



#' @rdname symbol2gene
#' @export
setMethod(
    "symbol2gene",
    signature("DataFrame"),
    .symbol2geneDim)



#' @rdname symbol2gene
#' @export
setMethod(
    "symbol2gene",
    signature("dgCMatrix"),
    .symbol2geneDim)



#' @rdname symbol2gene
#' @export
setMethod(
    "symbol2gene",
    signature("dgTMatrix"),
    .symbol2geneDim)



#' @rdname symbol2gene
#' @export
setMethod(
    "symbol2gene",
    signature("matrix"),
    .symbol2geneDim)
