#' Convert Gene Symbol to Ensembl Identifier
#'
#' @rdname symbol2gene
#' @name symbol2gene
#'
#' @param organism Organism name.
#'
#' @return Same class as original object.
#'
#' @seealso [detectOrganism()].
#'
#' @examples
#' # character
#' symbol2gene(c("Gnai3", "Pbsn"), organism = "mouse")
#'
#' # matrix
#' matrix(
#'     data = seq(1L:4L),
#'     byrow = TRUE,
#'     nrow = 2L,
#'     ncol = 2L,
#'     dimnames = list(c("Gnai3", "Pbsn"),
#'                     c("sample1", "sample2"))) %>%
#'     symbol2gene(organism = "mouse")
NULL



# Constructors ====
.s2gvec <- function(object, organism) {
    # Prevent pass in of organism as primary object.
    # Improve this in a future update.
    if (is_string(object)) {
        stop("symbol2gene conversion requires > 1 identifier")
    }
    if (any(is.na(object))) {
        stop("NA identifier detected", call. = FALSE)
    }
    if (any(object == "")) {
        stop("Empty string identifier detected", call. = FALSE)
    }

    # Detect organism
    organism <- detectOrganism(organism)

    # Get gene2symbol annotable
    g2s <- annotable(organism, format = "gene2symbol") %>%
        .[.[["symbol"]] %in% object, , drop = FALSE]

    ensgene <- g2s[["ensgene"]]
    names(ensgene) <- g2s[["symbol"]]
    if (!all(object %in% names(ensgene))) {
        nomatch <- setdiff(object, g2s[["symbol"]])
        names(nomatch) <- nomatch
        warning(paste(
            "Failed to match all gene symbols to IDs:",
            toString(nomatch)),
            call. = FALSE)
        ensgene <- c(ensgene, nomatch)
    }
    ensgene <- ensgene[object]

    # Final integrity checks
    if (any(is.na(ensgene))) {
        stop("NA genes detected", call. = FALSE)
    }
    if (any(duplicated(ensgene))) {
        stop("Duplicate genes detected", call. = FALSE)
    }

    ensgene
}



# Pass arguments to .s2gvec
.s2gdim <- function(object, organism) {
    rownames(object) <- rownames(object) %>%
        .s2gvec(organism = organism)
    object
}



# Methods ====
#' @rdname symbol2gene
#' @export
setMethod("symbol2gene", "character", .s2gvec)



#' @rdname symbol2gene
#' @export
setMethod("symbol2gene", "data.frame", .s2gdim)



#' @rdname symbol2gene
#' @export
setMethod("symbol2gene", "DataFrame", .s2gdim)



#' @rdname symbol2gene
#' @export
setMethod("symbol2gene", "dgCMatrix", .s2gdim)



#' @rdname symbol2gene
#' @export
setMethod("symbol2gene", "dgTMatrix", .s2gdim)



#' @rdname symbol2gene
#' @export
setMethod("symbol2gene", "matrix", .s2gdim)
