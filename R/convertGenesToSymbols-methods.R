#' Convert Ensembl Identifiers to Gene Symbols
#'
#' @name convertGenesToSymbols
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams makeGRangesFromEnsembl
#' @inheritParams general
#' @param organism `string`. Organism name. Normally this argument is
#'   unnecessary and can be left unset. If a count matrix starts with a FASTA
#'   spike-in (e.g. "EGFP"), then automatic genome detection based on the first
#'   gene identifier will fail. In this case, the desired organism must be
#'   manually declared.
#' @param gene2symbol `data.frame` or `NULL`. Gene-to-symbol mappings. If set
#'   `NULL`, the function will attempt to download the mappings from Ensembl
#'   automatically.
#' @param ... Passthrough to [makeGene2symbolFromEnsembl()].
#'
#' @return Same class as original object.
#'
#' @examples
#' # character ====
#' x <- c("ENSMUSG00000000001", "ENSMUSG00000000003")
#' convertGenesToSymbols(x)
#'
#' # matrix ====
#' mat <- matrix(
#'     data = seq(1L:4L),
#'     byrow = TRUE,
#'     nrow = 2L,
#'     ncol = 2L,
#'     dimnames = list(
#'         c("ENSMUSG00000000001", "ENSMUSG00000000003"),
#'         c("sample_1", "sample_2")
#'     )
#' )
#' print(mat)
#' mat <- convertGenesToSymbols(mat)
#' print(mat)
#' rownames(mat)
#'
#' # SummarizedExperiment ====
#' x <- convertGenesToSymbols(rse_bcb)
#' print(x)
#'
#' y <- convertSymbolsToGenes(x)
#' print(y)
NULL



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("character"),
    function(
        object,
        gene2symbol = NULL,
        ...
    ) {
        # Allowing duplicates here (unlike convertTranscriptsToGenes)
        assert_all_are_non_missing_nor_empty_character(object)
        assert_is_any_of(gene2symbol, c("data.frame", "NULL"))
        args <- list(...)
        organism <- args[["organism"]]

        # If no gene2symbol is provided, fall back to using Ensembl annotations
        if (is.null(gene2symbol)) {
            message("Obtaining gene-to-symbol mappings from Ensembl")
            if (is.null(organism)) {
                organism <- detectOrganism(object, unique = TRUE)
            }
            assert_is_a_string(organism)
            message(paste(organism, "genes detected"))
            args[["organism"]] <- organism
            gene2symbol <- do.call(
                what = makeGene2symbolFromEnsembl,
                args = args
            )
        }
        assertIsGene2symbol(gene2symbol)

        gene2symbol <- gene2symbol[
            match(object, gene2symbol[["geneID"]]),
            ,
            drop = FALSE
        ]

        return <- gene2symbol[["geneName"]]
        names(return) <- gene2symbol[["geneID"]]

        missing <- setdiff(object, gene2symbol[["geneID"]])
        if (length(missing)) {
            warning(paste("Failed to match genes:", toString(missing)))
            names(missing) <- missing
            return <- c(return, missing)
        }

        return[object]
    }
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("matrix"),
    function(object, ...) {
        rownames <- convertGenesToSymbols(rownames(object), ...)
        rownames(object) <- rownames
        object
    }
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("data.frame"),
    getMethod("convertGenesToSymbols", "matrix")
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("DataFrame"),
    getMethod("convertGenesToSymbols", "matrix")
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("dgCMatrix"),
    getMethod("convertGenesToSymbols", "matrix")
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("dgTMatrix"),
    getMethod("convertGenesToSymbols", "matrix")
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    "convertGenesToSymbols",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        gene2symbol <- gene2symbol(object)
        if (is.null(gene2symbol)) {
            return(object)
        }
        # Ensure factors get coerced to character.
        # Note that ".1" will be added here for duplicate gene symbols.
        symbols <- gene2symbol %>%
            .[, "geneName", drop = TRUE] %>%
            as.character() %>%
            make.unique()
        rownames(object) <- symbols
        object
    }
)
