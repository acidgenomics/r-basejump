#' Map Genes
#'
#' Take a user-defined gene vector and dynamically map the input to either the
#' object rownames or the gene names (symbols). These functions are useful for
#' writing code that needs to handle either gene identifier or gene name input
#' dynamically (e.g. for single-cell RNA-seq marker analysis).
#'
#' @section Ambiguous gene names:
#'
#' Some genomes (e.g. Homo sapiens, Mus musculus) contain duplicated gene names
#' for multiple gene identifiers. Normally we handle these ambiguous gene names
#' by sanitizing them with [base::make.names()]. If a user requests a gene name
#' that is duplicated, these functions will return a warning.
#'
#' @name mapGenes
#' @family Data Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param strict `boolean`. Require all genes to match. Recommended by default.
#'   If set `FALSE`, instead will return a warning to the user, and subset the
#'   genes vector to only include matches.
#'
#' @return `character`.
#'
#' @examples
#' object <- rse_small
#' print(object)
#'
#' rownames <- head(rownames(object))
#' print(rownames)
#'
#' geneIDs <- head(rowData(object)[["geneID"]])
#' print(geneIDs)
#'
#' geneNames <- head(as.character(rowData(object)[["geneName"]]))
#' print(geneNames)
#'
#' # Row names
#' mapGenesToRownames(object, genes = rownames)
#' mapGenesToRownames(object, genes = geneIDs)
#' mapGenesToRownames(object, genes = geneNames)
#'
#' # Gene identifiers
#' mapGenesToIDs(object, genes = rownames)
#' mapGenesToIDs(object, genes = geneIDs)
#' mapGenesToIDs(object, genes = geneNames)
#'
#' # Gene names (symbols)
#' mapGenesToSymbols(object, genes = rownames)
#' mapGenesToSymbols(object, genes = geneIDs)
#' mapGenesToSymbols(object, genes = geneNames)
NULL



.mapGenes <- function(object, genes, strict = TRUE) {
    assert_is_all_of(object, "gene2symbol")
    validObject(object)
    assert_is_character(genes)
    assert_is_non_empty(genes)
    assert_is_a_bool(strict)

    # Prepare the match table.
    if (any(genes %in% rownames(object))) {
        table <- rownames(object)
    } else if (any(genes %in% object[["geneName"]])) {
        assertAllAreUniqueGeneNames(object, genes)
        table <- object[["geneName"]]
    } else if (any(genes %in% object[["geneID"]])) {
        table <- object[["geneID"]]
    } else {
        stop(paste(
            "All genes failed to map:", toString(head(genes))
        ), call. = FALSE)
    }

    # Match the user input `genes` vector to the table.
    match <- match(x = genes, table = table)
    names(match) <- genes

    # Stop or warn if there are unmapped genes.
    if (isTRUE(strict)) {
        fun <- stop
    } else {
        fun <- warning
    }
    unmapped <- which(is.na(match))
    if (length(unmapped) > 0L) {
        fun(paste(
            "Some genes failed to map:", toString(genes[unmapped])
        ), call. = FALSE)
    }

    # Return the identifiers that map to rownames.
    mapped <- na.omit(match)
    assert_is_non_empty(mapped)
    mapped
}



# mapGenesToRownames ===========================================================
.mapGenesToRownames.gene2symbol <-  # nolint
    function(object, genes, strict = TRUE) {
        mapped <- do.call(
            what = .mapGenes,
            args = list(
                object = object,
                genes = genes,
                strict = strict
            )
        )
        return <- rownames(object[mapped, , drop = FALSE])
        return <- as.character(return)
        names(return) <- names(mapped)
        return
    }



.mapGenesToRownames.SE <-  # nolint
    function(object, genes, strict = TRUE) {
        validObject(object)
        g2s <- gene2symbol(object)
        assert_are_identical(rownames(g2s), rownames(object))
        do.call(
            what = mapGenesToRownames,
            args = list(
                object = g2s,
                genes = genes,
                strict = strict
            )
        )
    }



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToRownames",
    signature = signature("gene2symbol"),
    definition = .mapGenesToRownames.gene2symbol
)



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToRownames",
    signature = signature("SummarizedExperiment"),
    definition = .mapGenesToRownames.SE
)



# mapGenesToIDs ================================================================
.mapGenesToIDs.gene2symbol <-  # nolint
    function(object, genes, strict = TRUE) {
        mapped <- do.call(
            what = .mapGenes,
            args = list(
                object = object,
                genes = genes,
                strict = strict
            )
        )
        return <- object[mapped, , drop = FALSE]
        return <- return[["geneID"]]
        return <- as.character(return)
        names(return) <- names(mapped)
        return
    }



.mapGenesToIDs.SE <-  # nolint
    function(object, genes, strict = TRUE) {
        validObject(object)
        g2s <- gene2symbol(object)
        assert_are_identical(rownames(g2s), rownames(object))
        do.call(
            what = mapGenesToIDs,
            args = list(
                object = g2s,
                genes = genes,
                strict = strict
            )
        )
    }



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToIDs",
    signature = signature("gene2symbol"),
    definition = .mapGenesToIDs.gene2symbol
)



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToIDs",
    signature = signature("SummarizedExperiment"),
    definition = .mapGenesToIDs.SE
)



# mapGenesToSymbols ============================================================
.mapGenesToSymbols.gene2symbol <-  # nolint
    function(object, genes, strict = TRUE) {
        mapped <- do.call(
            what = .mapGenes,
            args = list(
                object = object,
                genes = genes,
                strict = strict
            )
        )
        return <- object[mapped, , drop = FALSE]
        return <- return[["geneName"]]
        return <- as.character(return)
        names(return) <- names(mapped)
        return
    }



.mapGenesToSymbols.SE <-  # nolint
    function(object, genes, strict = TRUE) {
        validObject(object)
        g2s <- gene2symbol(object)
        assert_are_identical(rownames(g2s), rownames(object))
        do.call(
            what = mapGenesToSymbols,
            args = list(
                object = g2s,
                genes = genes,
                strict = strict
            )
        )
    }



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToSymbols",
    signature = signature("gene2symbol"),
    definition = .mapGenesToSymbols.gene2symbol
)



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToSymbols",
    signature = signature("SummarizedExperiment"),
    definition = .mapGenesToSymbols.SE
)
