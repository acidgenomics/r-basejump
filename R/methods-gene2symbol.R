#' Convert Ensembl Identifier to Gene Symbol
#'
#' @rdname gene2symbol
#' @name gene2symbol
#'
#' @param genomeBuild *Optional*. Genome build. Normally this argument is
#'  unnecessary and can be left `NULL`. If a count matrix starts with a
#'  FASTA spike-in (e.g. "EGFP"), then automatic genome detection based on the
#'  first gene identifier will fail. In this case, the desired genome build
#'  should be manually declared.
#'
#' @return Same class as original object.
#' @export
#'
#' @examples
#' # character
#' c("ENSMUSG00000000001",
#'   "ENSMUSG00000000003") %>% gene2symbol
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
#'                       "sample2"))) %>% gene2symbol
NULL



# Constructors ====
.g2svec <- function(object, genomeBuild = NULL) {
    # Prevent pass in of genomeBuild as primary object.
    # Improve this in a future update.
    if (is_string(object)) {
        stop("gene2symbol conversion requires > 1 identifier")
    }
    if (any(is.na(object))) {
        stop("NA identifier detected", call. = FALSE)
    }
    if (any(object == "")) {
        stop("Empty string identifier detected", call. = FALSE)
    }

    # Detect organism
    if (!is.null(genomeBuild)) {
        organism <- detectOrganism(genomeBuild)
    } else {
        organism <- detectOrganism(object[[1L]])
    }

    g2s <- annotable(organism, format = "gene2symbol") %>%
        .[object, ] %>%
        .[!is.na(.[["symbol"]]), ]

    symbol <- g2s[["symbol"]]
    names(symbol) <- g2s[["ensgene"]]
    if (!all(object %in% names(symbol))) {
        nomatch <- setdiff(object, rownames(g2s))
        names(nomatch) <- nomatch
        warning(paste(
            "Failed to match all gene IDs to symbols:",
            toString(nomatch)),
            call. = FALSE)
        symbol <- c(symbol, nomatch)
    }
    symbol <- symbol[object]

    # Final integrity checks
    if (any(is.na(symbol))) {
        stop("NA symbols detected", call. = FALSE)
    }
    if (any(duplicated(symbol))) {
        stop("Duplicate symbols detected", call. = FALSE)
    }

    symbol
}



# Pass arguments to .g2svec
.g2sdim <- function(object, genomeBuild = NULL) {
    rownames(object) <- rownames(object) %>%
        .g2svec(genomeBuild = genomeBuild)
    object
}



# Methods ====
#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "character", .g2svec)



#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "data.frame", .g2sdim)



#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "DataFrame", .g2sdim)



#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "dgCMatrix", .g2sdim)



#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "dgTMatrix", .g2sdim)



#' @rdname gene2symbol
#' @export
setMethod("gene2symbol", "matrix", .g2sdim)
