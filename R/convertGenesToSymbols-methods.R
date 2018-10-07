# FIXME Speed up the working example.



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
#' @param gene2symbol `Gene2Symbol` or `NULL`. Gene-to-symbol mappings. If set
#'   `NULL`, the function will attempt to download the mappings from Ensembl
#'   automatically.
#'
#' @return Object with gene IDs converted to names (symbols).
#'
#' @examples
#' gene2symbol <- makeGene2symbolFromEnsembl("Mus musculus")
#' print(gene2symbol)
#' geneIDs <- c("ENSMUSG00000000001", "ENSMUSG00000000003")
#' sampleIDs = paste0("sample", seq_len(2L))
#'
#' # character ====
#' x <- convertGenesToSymbols(geneIDs, gene2symbol = gene2symbol)
#' print(x)
#'
#' # matrix ====
#' counts <- matrix(
#'     data = seq_len(length(geneIDs) * length(sampleIDs)),
#'     byrow = TRUE,
#'     nrow = length(geneIDs),
#'     ncol = length(sampleIDs),
#'     dimnames = list(geneIDs, sampleIDs)
#' )
#' print(counts)
#' x <- convertGenesToSymbols(counts, gene2symbol = gene2symbol)
#' print(x)
#'
#' # SummarizedExperiment ====
#' x <- convertGenesToSymbols(rse_small)
#' print(x)
#' # Interconvert back to gene IDs.
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
        assert_is_any_of(gene2symbol, c("Gene2Symbol", "NULL"))
        assertIsAStringOrNULL(organism)

        # If no gene2symbol is provided, fall back to using Ensembl annotations.
        if (is.null(gene2symbol)) {
            message("Obtaining gene-to-symbol mappings from Ensembl...")
            if (is.null(organism)) {
                organism <- organism(object)
            }
            assert_is_a_string(organism)
            message(paste(organism, "genes detected."))
            gene2symbol <- do.call(
                what = makeGene2symbolFromEnsembl,
                args = matchArgsToDoCall(
                    args = list(organism = organism),
                    removeFormals = c("object", "gene2symbol")
                )
            )
        }
        assert_is_all_of(gene2symbol, "Gene2Symbol")

        # Arrange the gene2symbol to match the input.
        gene2symbol <- gene2symbol[
            match(x = object, table = gene2symbol[["geneID"]]),
            ,
            drop = FALSE
            ]

        return <- gene2symbol[["geneName"]]
        names(return) <- gene2symbol[["geneID"]]

        missing <- setdiff(object, gene2symbol[["geneID"]])
        if (has_length(missing)) {
            warning(paste(
                "Failed to match genes:", toString(missing)
            ), call. = FALSE)
            names(missing) <- missing
            return <- c(return, missing)
        }

        return[object]
    }
f1 <- formals(.convertGenesToSymbols.character)
f2 <- formals(makeGRangesFromEnsembl)
f2 <- f2[setdiff(names(f2), c(names(f1), "level"))]
f <- c(f1, f2)
formals(.convertGenesToSymbols.character) <- f



.convertGenesToSymbols.matrix <-  # nolint
    function() {
        g2s <- do.call(
            what = convertGenesToSymbols,
            args = matchArgsToDoCall(
                args = list(object = rownames(object))
            )
        )
        rownames(object) <- as.character(g2s)
        object
    }
f <- formals(.convertGenesToSymbols.character)
formals(.convertGenesToSymbols.matrix) <- f



.convertGenesToSymbols.SE <-  # nolint
    function(object) {
        validObject(object)
        g2s <- gene2symbol(object)
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
    definition = .convertGenesToSymbols.SE
)



# convertSymbolsToGenes ========================================================
.convertSymbolsToGenes.SE <-  # nolint
    function(object) {
        validObject(object)
        g2s <- gene2symbol(object)
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
    definition = .convertSymbolsToGenes.SE
)
