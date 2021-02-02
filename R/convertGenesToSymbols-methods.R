#' @name convertGenesToSymbols
#' @inherit AcidGenerics::convertGenesToSymbols
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object of same class.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#' object <- rse
#'
#' g2s <- Gene2Symbol(object)
#' print(g2s)
#' genes <- head(g2s[["geneId"]])
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



## Allowing duplicates here (unlike convertTranscriptsToGenes).
## Updated 2021-02-02.
`convertGenesToSymbols,character` <-  # nolint
    function(object, gene2symbol) {
        assert(
            isCharacter(object),
            is(gene2symbol, "Gene2Symbol")
        )
        ## Arrange the gene2symbol to match the input.
        cols <- c("geneId", "geneName")
        if (!identical(cols, colnames(gene2symbol))) {
            colnames(gene2symbol) <- cols
        }
        validObject(gene2symbol)
        gene2symbol <- gene2symbol[
            match(x = object, table = gene2symbol[["geneId"]]),
            ,
            drop = FALSE
        ]
        out <- gene2symbol[["geneName"]]
        names(out) <- gene2symbol[["geneId"]]
        missing <- setdiff(object, gene2symbol[["geneId"]])
        if (hasLength(missing)) {
            warning(sprintf(
                "Failed to match genes: %s.",
                toString(missing, width = 100L)
            ))
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
    definition = `convertGenesToSymbols,character`
)



## Updated 2021-01-17.
`convertGenesToSymbols,matrix` <-  # nolint
    function(object, gene2symbol) {
        gene2symbol <- convertGenesToSymbols(
            object = rownames(object),
            gene2symbol = gene2symbol
        )
        rownames(object) <- as.character(gene2symbol)
        object
    }



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("matrix"),
    definition = `convertGenesToSymbols,matrix`
)



## Updated 2020-01-30.
`convertGenesToSymbols,Matrix` <-  # nolint
    `convertGenesToSymbols,matrix`



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("Matrix"),
    definition = `convertGenesToSymbols,Matrix`
)



## Updated 2021-01-17.
`convertGenesToSymbols,GRanges` <-  # nolint
    function(object) {
        validObject(object)
        gene2symbol <- Gene2Symbol(object)
        symbols <- gene2symbol[[2L]]
        assert(hasNoDuplicates(symbols))
        names(object) <- as.character(symbols)
        object
    }



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("GRanges"),
    definition = `convertGenesToSymbols,GRanges`
)



## Updated 2021-01-17.
`convertGenesToSymbols,SummarizedExperiment` <-  # nolint
    function(object) {
        validObject(object)
        gene2symbol <- Gene2Symbol(object)
        symbols <- gene2symbol[[2L]]
        assert(hasNoDuplicates(symbols))
        rownames(object) <- as.character(symbols)
        if (is(object, "RangedSummarizedExperiment")) {
            assert(identical(rownames(object), names(rowRanges(object))))
        }
        object
    }



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("SummarizedExperiment"),
    definition = `convertGenesToSymbols,SummarizedExperiment`
)



## Updated 2021-01-17.
`convertSymbolsToGenes,SummarizedExperiment` <-  # nolint
    function(object) {
        validObject(object)
        gene2symbol <- Gene2Symbol(object)
        assert(
            identical(rownames(object), gene2symbol[[2L]]),
            hasNoDuplicates(gene2symbol[[1L]])
        )
        rownames(object) <- as.character(gene2symbol[[1L]])
        object
    }



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertSymbolsToGenes",
    signature = signature("SummarizedExperiment"),
    definition = `convertSymbolsToGenes,SummarizedExperiment`
)
