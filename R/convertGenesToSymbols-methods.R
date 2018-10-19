# FIXME Working example with devtools isn't capturing gene2symbol correctly.



#' Convert Ensembl Identifiers to Gene Symbols
#'
#' @name convertGenesToSymbols
#' @family Identifier Mapping Functions
#'
#' @inheritParams general
#'
#' @return Object with gene IDs converted to names (symbols).
#'
#' @examples
#' data(rse_small)
#' object <- rse_small
#'
#' g2s <- Gene2Symbol(object)
#' print(g2s)
#' genes <- head(g2s[["geneID"]])
#' print(genes)
#'
#' ## character ====
#' x <- convertGenesToSymbols(genes, gene2symbol = g2s)
#' print(x)
#'
#' ## matrix ====
#' samples <- head(colnames(object))
#' counts <- matrix(
#'     data = seq_len(length(genes) * length(samples)),
#'     byrow = TRUE,
#'     nrow = length(genes),
#'     ncol = length(samples),
#'     dimnames = list(genes, samples)
#' )
#' print(counts)
#' x <- convertGenesToSymbols(counts, gene2symbol = g2s)
#' print(x)
#'
#' ## SummarizedExperiment ====
#' x <- convertGenesToSymbols(rse_small)
#' print(x)
#' ## Interconvert back to gene IDs.
#' y <- convertSymbolsToGenes(x)
#' print(y)
NULL



# convertGenesToSymbols ========================================================
# Allowing duplicates here (unlike convertTranscriptsToGenes).
.convertGenesToSymbols.character <-  # nolint
    function(object, gene2symbol) {
        assert_all_are_non_missing_nor_empty_character(object)
        assert_is_all_of(gene2symbol, "Gene2Symbol")
        validObject(gene2symbol)

        # Arrange the gene2symbol to match the input.
        gene2symbol <- gene2symbol[
            match(x = object, table = gene2symbol[["geneID"]]),
            ,
            drop = FALSE
        ]

        out <- gene2symbol[["geneName"]]
        names(out) <- gene2symbol[["geneID"]]

        missing <- setdiff(object, gene2symbol[["geneID"]])
        if (has_length(missing)) {
            warning(paste(
                "Failed to match genes:", toString(missing)
            ), call. = FALSE)
            names(missing) <- missing
            out <- c(out, missing)
        }

        out[object]
    }



.convertGenesToSymbols.matrix <-  # nolint
    function(object, gene2symbol) {
        g2s <- do.call(
            what = convertGenesToSymbols,
            args = list(
                object = rownames(object),
                gene2symbol = gene2symbol
            )
        )
        rownames(object) <- as.character(g2s)
        object
    }



.convertGenesToSymbols.SummarizedExperiment <-  # nolint
    function(object) {
        validObject(object)
        g2s <- Gene2Symbol(object)
        symbols <- g2s[["geneName"]]
        assert_has_no_duplicates(symbols)
        # Update the object rownames.
        rownames(object) <- as.character(symbols)
        # Ensure all names get updated correctly.
        if (is(object, "RangedSummarizedExperiment")) {
            assert_are_identical(
                x = rownames(object),
                y = names(rowRanges(object))
            )
        }
        object
    }



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
    signature = signature("sparseMatrix"),
    definition = getMethod("convertGenesToSymbols", "matrix")
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("SummarizedExperiment"),
    definition = .convertGenesToSymbols.SummarizedExperiment
)



# convertSymbolsToGenes ========================================================
.convertSymbolsToGenes.SummarizedExperiment <-  # nolint
    function(object) {
        validObject(object)
        g2s <- Gene2Symbol(object)
        assert_are_identical(
            x = rownames(object),
            y = g2s[["geneName"]]
        )
        assert_has_no_duplicates(g2s[["geneID"]])
        rownames(object) <- as.character(g2s[["geneID"]])
        object
    }



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertSymbolsToGenes",
    signature = signature("SummarizedExperiment"),
    definition = .convertSymbolsToGenes.SummarizedExperiment
)
