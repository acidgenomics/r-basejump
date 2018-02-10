#' Convert Ensembl Identifier to Gene Symbol
#'
#' @rdname convertGenesToSymbols
#' @name convertGenesToSymbols
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
#' convertGenesToSymbols(vec)
#'
#' # matrix
#' mat <- matrix(
#'     data = seq(1L:4L),
#'     byrow = TRUE,
#'     nrow = 2L,
#'     ncol = 2L,
#'     dimnames = list(
#'         c("ENSMUSG00000000001", "ENSMUSG00000000003"),
#'         c("sample1", "sample2")
#'     )
#' )
#' convertGenesToSymbols(mat)
NULL



# Constructors =================================================================
.convertGenesToSymbols <- function(  # nolint
    object,
    organism,
    release = NULL,
    quiet = FALSE) {
    assert_is_character(object)
    .assert_is_numeric_scalar_or_null(release)
    assert_is_a_bool(quiet)

    if (any(is.na(object))) {
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
    assert_is_a_string(organism)

    gene2symbol <- annotable(
        organism,
        format = "gene2symbol",
        release = release,
        quiet = quiet) %>%
        .[object, , drop = FALSE] %>%
        .[!is.na(.[["symbol"]]), , drop = FALSE]

    symbol <- gene2symbol[["symbol"]]
    names(symbol) <- gene2symbol[["ensgene"]]

    if (!all(object %in% names(symbol))) {
        nomatch <- setdiff(object, rownames(gene2symbol))
        names(nomatch) <- nomatch
        warn(paste(
            "Failed to match all genes to symbols:",
            toString(nomatch)
        ))
        symbol <- c(symbol, nomatch)
    }

    assert_is_character(symbol)
    assert_has_names(symbol)
    symbol[object]
}



.convertGenesToSymbols.dim <- function(  # nolint
    object,
    organism,
    release = NULL,
    quiet = FALSE) {
    # Passthrough: organism, release, quiet
    assert_has_rownames(object)
    rownames <- rownames(object)
    rownames <- .convertGenesToSymbols(
        object = rownames,
        organism = organism,
        release = release,
        quiet = quiet)
    rownames(object) <- rownames
    object
}



# Methods ======================================================================
#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("character"),
    .convertGenesToSymbols)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("data.frame"),
    .convertGenesToSymbols.dim)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("DataFrame"),
    .convertGenesToSymbols.dim)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("dgCMatrix"),
    .convertGenesToSymbols.dim)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("dgTMatrix"),
    .convertGenesToSymbols.dim)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("matrix"),
    .convertGenesToSymbols.dim)
