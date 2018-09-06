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
#'
#' @return `character`.
#'
#' @examples
#' object <- rse_small
#' head(rownames(object))
#'
#' object_symbols <- convertGenesToSymbols(object)
#' head(rownames(object_symbols))
#'
#' geneIDs <- head(rownames(object))
#' print(geneIDs)
#'
#' geneNames <- head(as.character(rowRanges(object)$geneName))
#' print(geneNames)
#'
#' # Row names
#' mapGenesToRownames(object, genes = geneIDs)
#' mapGenesToRownames(object, genes = geneNames)
#' mapGenesToRownames(object_symbols, genes = geneIDs)
#' mapGenesToRownames(object_symbols, genes = geneNames)
#'
#' # Gene identifiers
#' mapGenesToIDs(object, genes = geneIDs)
#' mapGenesToIDs(object, genes = geneNames)
#' mapGenesToIDs(object_symbols, genes = geneIDs)
#' mapGenesToIDs(object_symbols, genes = geneNames)
#'
#' # Gene names (symbols)
#' mapGenesToSymbols(object, genes = geneIDs)
#' mapGenesToSymbols(object, genes = geneNames)
#' mapGenesToSymbols(object_symbols, genes = geneIDs)
#' mapGenesToSymbols(object_symbols, genes = geneNames)
NULL



.mapGenesError <- "Failed to map genes"



# mapGenesToRownames ===========================================================
#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToRownames",
    signature = signature("DataFrame"),
    definition = function(object, genes) {
        assertIsGene2symbol(object)
        if (any(genes %in% object[["geneName"]])) {
            # User passed in gene names, but the object uses gene IDs.
            assert_is_subset(genes, object[["geneName"]])
            # FIXME Rethink how to approach this check.
            # assertAllAreUniqueGeneNames(object, genes)
            match <- match(x = genes, table = object[["geneName"]])
            assert_all_are_not_na(match)
            return <- rownames(object[match, , drop = FALSE])
        } else if (any(genes %in% object[["geneID"]])) {
            # User passed in gene IDs, but the object uses gene names.
            # This scenario isn't common but we're supporting it anyway.
            assert_is_subset(genes, object[["geneID"]])
            match <- match(x = genes, table = object[["geneID"]])
            assert_all_are_not_na(match)
            return <- rownames(object[match, , drop = FALSE])
        } else {
            stop(.mapGenesError)
        }
        return <- as.character(return)
        names(return) <- genes
        return
    }
)



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToRownames",
    signature = signature("SummarizedExperiment"),
    definition = function(object, genes) {
        validObject(object)
        assert_is_character(genes)
        assert_is_non_empty(genes)
        # Return early if the `genes` vector already matches rownames.
        if (all(genes %in% rownames(object))) {
            return(genes)
        }
        # Get the gene-to-symbol mappings.
        g2s <- gene2symbol(object)
        assert_are_identical(rownames(g2s), rownames(object))
        # Using the DataFrame method.
        mapGenesToRownames(object = g2s, genes = genes)
    }
)



# mapGenesToIDs ================================================================
#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToIDs",
    signature = signature("DataFrame"),
    definition = function(object, genes) {
        assertIsGene2symbol(object)
        if (any(genes %in% object[["geneID"]])) {
            # User passed in gene identifiers.
            assert_is_subset(genes, object[["geneID"]])
            return(genes)
        } else if (any(genes %in% object[["geneName"]])) {
            # User passed in gene names.
            assert_is_subset(genes, object[["geneName"]])
            # FIXME Rethink this step.
            # assertAllAreUniqueGeneNames(object, genes)
            match <- match(x = genes, table = object[["geneName"]])
            assert_all_are_not_na(match)
            return <- object[match, "geneID", drop = TRUE]
        } else {
            stop(.mapGenesError)
        }
        return <- as.character(return)
        names(return) <- genes
        return
    }
)



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToIDs",
    signature = signature("SummarizedExperiment"),
    definition = function(object, genes) {
        validObject(object)
        assert_is_character(genes)
        assert_is_non_empty(genes)
        # Get the gene-to-symbol mappings.
        g2s <- gene2symbol(object)
        assert_are_identical(rownames(g2s), rownames(object))
        # Using DataFrame method.
        mapGenesToIDs(object = g2s, genes = genes)
    }
)



# mapGenesToSymbols ============================================================
#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToSymbols",
    signature = signature("DataFrame"),
    definition = function(object, genes) {
        assertIsGene2symbol(object)
        if (any(genes %in% object[["geneID"]])) {
            # User passed in gene identifiers.
            assert_is_subset(genes, object[["geneID"]])
            match <- match(x = genes, table = object[["geneID"]])
            assert_all_are_not_na(match)
            return <- object[match, "geneName", drop = TRUE]
        } else if (any(genes %in% object[["geneName"]])) {
            # User passed in gene names.
            assert_is_subset(genes, object[["geneName"]])
            # FIXME Rethink this step.
            # assertAllAreUniqueGeneNames(object, genes)
            return(genes)
        } else {
            stop(.mapGenesError)
        }
        return <- as.character(return)
        names(return) <- genes
        return
    }
)



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToSymbols",
    signature = signature("SummarizedExperiment"),
    definition = function(object, genes) {
        validObject(object)
        assert_is_character(genes)
        assert_is_non_empty(genes)
        # Get the gene-to-symbol mappings.
        g2s <- gene2symbol(object)
        if (is.null(g2s)) {
            stop(.mapGenesError)
        }
        assert_are_identical(rownames(g2s), rownames(object))
        # Using the DataFrame method.
        mapGenesToSymbols(object = g2s, genes = genes)
    }
)
