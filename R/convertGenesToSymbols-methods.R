#' Convert Ensembl Identifiers to Gene Symbols
#'
#' @name convertGenesToSymbols
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @include makeGRanges.R
#'
#' @inheritParams makeGRangesFromEnsembl
#' @inheritParams general
#' @param gene2symbol `data.frame` or `NULL`. Gene-to-symbol mappings. If set
#'   `NULL`, the function will attempt to download the mappings from Ensembl
#'   automatically.
#'
#' @return Same class as original object.
#'
#' @examples
#' # character ====
#' from <- c("ENSMUSG00000000001", "ENSMUSG00000000003")
#' to <- convertGenesToSymbols(from)
#' print(to)
#'
#' # matrix ====
#' from <- matrix(
#'     data = seq(1L:4L),
#'     byrow = TRUE,
#'     nrow = 2L,
#'     ncol = 2L,
#'     dimnames = list(
#'         c("ENSMUSG00000000001", "ENSMUSG00000000003"),
#'         c("sample1", "sample2")
#'     )
#' )
#' print(from)
#' to <- convertGenesToSymbols(from)
#' print(to)
#' rownames(to)
#'
#' # SummarizedExperiment ====
#' x <- convertGenesToSymbols(rse_small)
#' print(x)
#' y <- convertSymbolsToGenes(x)
#' print(y)
NULL



# convertGenesToSymbols ========================================================
.convertGenesToSymbols.character <-  # nolint
function(
    # Setting formals below.
    object,
    gene2symbol = NULL,
    organism = NULL
) {
    # Allowing duplicates here (unlike convertTranscriptsToGenes).
    assert_all_are_non_missing_nor_empty_character(object)
    assert_is_any_of(gene2symbol, c("DataFrame", "NULL"))
    assertIsAStringOrNULL(organism)

    # If no gene2symbol is provided, fall back to using Ensembl annotations.
    if (is.null(gene2symbol)) {
        message("Obtaining gene-to-symbol mappings from Ensembl")
        if (is.null(organism)) {
            organism <- detectOrganism(object, unique = TRUE)
        }
        assert_is_a_string(organism)
        message(paste(organism, "genes detected"))
        gene2symbol <- do.call(
            what = makeGene2symbolFromEnsembl,
            args = matchArgsToDoCall(
                args = list(organism = organism),
                removeFormals = c("object", "gene2symbol"),
                call = standardizeCall(verbose = TRUE),
                verbose = TRUE
            )
        )
    }
    assertIsGene2symbol(gene2symbol)

    # Arrange the gene2symbol to match the input.
    gene2symbol <- gene2symbol[
        match(x = object, table = gene2symbol[["geneID"]]),
        ,
        drop = FALSE
    ]

    return <- gene2symbol[["geneName"]]
    names(return) <- gene2symbol[["geneID"]]

    missing <- setdiff(object, gene2symbol[["geneID"]])
    if (length(missing)) {
        warning(paste(
            "Failed to match genes:", toString(missing)
        ), call. = FALSE)
        names(missing) <- missing
        return <- c(return, missing)
    }

    return[object]
}

# Set the formals.
f1 <- formals(.convertGenesToSymbols.character)
f2 <- formals(makeGRangesFromEnsembl)
f2 <- f2[setdiff(
    x = names(f2),
    y = c(names(f1), "format", "metadata")
)]
f <- c(f1, f2)
formals(.convertGenesToSymbols.character) <- f



.convertGenesToSymbols.matrix <-  # nolint
function(
    # Setting formals below.
) {
    rownames <- rownames(object)
    rownames <- do.call(
        what = convertGenesToSymbols,
        args = matchArgsToDoCall(
            args = list(object = rownames),
            call = standardizeCall(verbose = TRUE),
            verbose = TRUE
        )
    )
    rownames(object) <- rownames
    object
}

# Set the formals.
f <- formals(.convertGenesToSymbols.character)
formals(.convertGenesToSymbols.matrix) <- f



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("character"),
    .convertGenesToSymbols.character
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("matrix"),
    .convertGenesToSymbols.matrix
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("data.frame"),
    definition = getMethod("convertGenesToSymbols", "matrix")
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("DataFrame"),
    definition = getMethod("convertGenesToSymbols", "matrix")
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("dgCMatrix"),
    definition = getMethod("convertGenesToSymbols", "matrix")
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("dgTMatrix"),
    definition = getMethod("convertGenesToSymbols", "matrix")
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("SummarizedExperiment"),
    definition = function(object) {
        validObject(object)
        g2s <- gene2symbol(object)
        symbols <- g2s[["geneName"]]
        assert_has_no_duplicates(symbols)
        # Update the object rownames.
        rownames(object) <- symbols
        # Ensure all names get updated correctly.
        if (is(object, "RangedSummarizedExperiment")) {
            assert_are_identical(
                x = rownames(object),
                y = names(rowRanges(object))
            )
        }

        object
    }
)



# convertSymbolsToGenes ========================================================
#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertSymbolsToGenes",
    signature = signature("SummarizedExperiment"),
    definition = function(object) {
        validObject(object)
        g2s <- gene2symbol(object)
        if (is.null(g2s)) {
            stop("Object does not contain gene-to-symbol mappings")
        }
        assert_are_identical(rownames(object), g2s[["geneName"]])
        assert_has_no_duplicates(g2s[["geneID"]])
        rownames(object) <- g2s[["geneID"]]
        object
    }
)
