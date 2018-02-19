#' Convert Ensembl Identifier to Gene Symbol
#'
#' @rdname convertGenesToSymbols
#' @name convertGenesToSymbols
#' @family Gene Annotation Utilities
#'
#' @inheritParams general
#' @inheritParams annotable
#'
#' @param gene2symbol *Optional.* Gene-to-symbol mappings. If `NULL`, will
#'   attempt to download from Ensembl using the desired `organism`,
#'   `genomeBuild`, and `release` arguments.
#' @param organism *Optional.* Organism name. Normally this argument is
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
    gene2symbol = NULL,
    organism = NULL,
    genomeBuild = NULL,
    release = NULL,
    uniqueSymbol = FALSE,
    quiet = FALSE) {
    # Passthrough: genomeBuild, release, quiet
    assert_is_character(object)
    assert_all_are_non_missing_nor_empty_character(object)
    assert_has_no_duplicates(object)
    assertIsDataFrameOrNull(gene2symbol)
    assertIsAStringOrNULL(organism)
    assert_is_a_bool(uniqueSymbol)

    # If no gene2symbol is provided, fall back to using Ensembl annotations
    if (!is.data.frame(gene2symbol)) {
        # Generate gene2symbol from Ensembl
        if (isTRUE(quiet)) {
            inform("Obtaining gene-to-symbol mappings from Ensembl")
        }
        if (is.null(organism)) {
            organism <- detectOrganism(object, unique = TRUE)
        } else if (is_a_string(organism)) {
            organism <- detectOrganism(organism)
        }
        assert_is_a_string(organism)
        gene2symbol <- gene2symbol(
            object = organism,
            genomeBuild = genomeBuild,
            release = release,
            uniqueSymbol = uniqueSymbol,
            quiet = quiet)
    } else {
        assertIsGene2symbol(gene2symbol)
        if (isTRUE(uniqueSymbol)) {
            gene2symbol[["symbol"]] <- make.unique(gene2symbol[["symbol"]])
        }
    }

    gene2symbol <- gene2symbol %>%
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
    gene2symbol = NULL,
    genomeBuild = NULL,
    organism = NULL,
    release = NULL,
    uniqueSymbol = FALSE,
    quiet = FALSE) {
    # Passthrough: gene2symbol, organism, release, quiet
    assertHasRownames(object)
    rownames <- rownames(object)
    rownames <- .convertGenesToSymbols(
        object = rownames,
        gene2symbol = gene2symbol,
        genomeBuild = genomeBuild,
        organism = organism,
        release = release,
        uniqueSymbol = uniqueSymbol,
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
