#' Convert Ensembl Identifier to Gene Symbol
#'
#' @rdname gene2symbol
#' @name gene2symbol
#'
#' @return Same class as original object.
#' @export
#'
#' @examples
#' # character
#' c("ENSMUSG00000000000",
#'   "ENSMUSG00000000001",
#'   "ENSMUSG00000000003") %>% gene2symbol
#'
#' # matrix
#' matrix(
#'     data = seq(1:4),
#'     byrow = TRUE,
#'     nrow = 2,
#'     ncol = 2,
#'     dimnames = list(c("ENSMUSG00000000001",
#'                       "ENSMUSG00000000003"),
#'                     c("sample1",
#'                       "sample2"))) %>% gene2symbol
NULL



# Constructors ====
.g2svec <- function(object) {
    if (!is.vector(object)) stop("Object must be vector")
    organism <- detectOrganism(object[[1L]])
    g2s <- annotable(organism, format = "gene2symbol") %>%
        .[object, ] %>%
        .[!is.na(.[["symbol"]]), ]
    symbol <- g2s[["symbol"]]
    names(symbol) <- g2s[["ensgene"]]
    if (!all(object %in% names(symbol))) {
        warning("Unmatched genes present")
        nomatch <- setdiff(object, rownames(g2s))
        names(nomatch) <- nomatch
        vec <- c(symbol, nomatch)[object]
    }

    # Final integrity checks
    if (any(is.na(vec))) stop("NA symbols detected")
    if (any(duplicated(vec))) stop("Duplicate symbols detected")

    vec
}



.g2srow <- function(object) {
    rownames(object) <- rownames(object) %>% .g2svec
    object
}



# Methods ====
#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "character", .g2svec)

#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "data.frame", .g2srow)

#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "DataFrame", .g2srow)

#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "dgCMatrix", .g2srow)

#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "dgTMatrix", .g2srow)

#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "matrix", .g2srow)
