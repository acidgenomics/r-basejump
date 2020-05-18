#' @name convertGenesToSymbols
#' @inherit acidgenerics::convertGenesToSymbols
#' @note Updated 2020-05-17.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object of same class.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
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



#' @rdname convertGenesToSymbols
#' @name convertGenesToSymbols
#' @importFrom acidgenerics convertGenesToSymbols
#' @usage convertGenesToSymbols(object, ...)
#' @export
NULL

#' @rdname convertGenesToSymbols
#' @name convertSymbolsToGenes
#' @importFrom acidgenerics convertSymbolsToGenes
#' @usage convertSymbolsToGenes(object, ...)
#' @export
NULL



## Allowing duplicates here (unlike convertTranscriptsToGenes).
## Updated 2020-05-17.
`convertGenesToSymbols,character` <-  # nolint
    function(object, gene2symbol) {
        assert(
            isCharacter(object),
            is(gene2symbol, "Gene2Symbol")
        )
        validObject(gene2symbol)
        ## Arrange the gene2symbol to match the input.
        gene2symbol <- gene2symbol[
            match(x = object, table = gene2symbol[["geneID"]]),
            ,
            drop = FALSE
        ]
        out <- gene2symbol[["geneName"]]
        names(out) <- gene2symbol[["geneID"]]
        missing <- setdiff(object, gene2symbol[["geneID"]])
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



## Updated 2019-07-22.
`convertGenesToSymbols,matrix` <-  # nolint
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



## Updated 2020-05-17.
`convertGenesToSymbols,GRanges` <-  # nolint
    function(object) {
        validObject(object)
        g2s <- Gene2Symbol(object)
        symbols <- g2s[["geneName"]]
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



## Updated 2019-07-22.
`convertGenesToSymbols,SummarizedExperiment` <-  # nolint
    function(object) {
        validObject(object)
        g2s <- Gene2Symbol(object)
        symbols <- g2s[["geneName"]]
        assert(hasNoDuplicates(symbols))
        ## Update the object rownames.
        rownames(object) <- as.character(symbols)
        ## Ensure all names get updated correctly.
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



## Updated 2019-07-22.
`convertSymbolsToGenes,SummarizedExperiment` <-  # nolint
    function(object) {
        validObject(object)
        g2s <- Gene2Symbol(object)
        assert(
            identical(rownames(object), g2s[["geneName"]]),
            hasNoDuplicates(g2s[["geneID"]])
        )
        rownames(object) <- as.character(g2s[["geneID"]])
        object
    }



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertSymbolsToGenes",
    signature = signature("SummarizedExperiment"),
    definition = `convertSymbolsToGenes,SummarizedExperiment`
)
