#' Convert Genes to Symbols
#'
#' @name convertGenesToSymbols
#' @inheritParams params
#'
#' @return Modified object of same class.
#'
#' @examples
#' data(rse)
#' object <- rse
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
#' x <- convertGenesToSymbols(rse)
#' print(x)
#' ## Interconvert back to gene IDs.
#' y <- convertSymbolsToGenes(x)
#' print(y)
NULL



# convertGenesToSymbols ========================================================
# Allowing duplicates here (unlike convertTranscriptsToGenes).
convertGenesToSymbols.character <-  # nolint
    function(object, gene2symbol) {
        assertAllAreNonMissingNorEmptyCharacter(object)
        assertClass(gene2symbol, "Gene2Symbol")
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
        if (length(missing) > 0L) {
            warning(paste(
                "Failed to match genes:", toString(missing)
            ), call. = FALSE)
            names(missing) <- missing
            out <- c(out, missing)
        }

        out[object]
    }



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("character"),
    convertGenesToSymbols.character
)



convertGenesToSymbols.matrix <-  # nolint
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



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("matrix"),
    definition = convertGenesToSymbols.matrix
)


convertGenesToSymbols.sparseMatrix <-  # nolint
    convertGenesToSymbols.matrix

#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("sparseMatrix"),
    definition = convertGenesToSymbols.sparseMatrix
)



convertGenesToSymbols.SummarizedExperiment <-  # nolint
    function(object) {
        validObject(object)
        g2s <- Gene2Symbol(object)
        symbols <- g2s[["geneName"]]
        assertHasNoDuplicates(symbols)
        # Update the object rownames.
        rownames(object) <- as.character(symbols)
        # Ensure all names get updated correctly.
        if (is(object, "RangedSummarizedExperiment")) {
            assertIdentical(
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
    signature = signature("SummarizedExperiment"),
    definition = convertGenesToSymbols.SummarizedExperiment
)



# convertSymbolsToGenes ========================================================
convertSymbolsToGenes.SummarizedExperiment <-  # nolint
    function(object) {
        validObject(object)
        g2s <- Gene2Symbol(object)
        assertIdentical(
            x = rownames(object),
            y = g2s[["geneName"]]
        )
        assertHasNoDuplicates(g2s[["geneID"]])
        rownames(object) <- as.character(g2s[["geneID"]])
        object
    }



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertSymbolsToGenes",
    signature = signature("SummarizedExperiment"),
    definition = convertSymbolsToGenes.SummarizedExperiment
)
