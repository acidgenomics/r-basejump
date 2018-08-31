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
#' object <- rse_bcb
#' head(rownames(object))
#'
#' object_symbols <- convertGenesToSymbols(object)
#' head(rownames(object_symbols))
#'
#' geneIDs <- head(rownames(object))
#' geneNames <- head(as.character(rowRanges(object)$geneName))
#'
#' mapGenesToRownames(object, genes = geneIDs)
#' mapGenesToRownames(object, genes = geneNames)
#' mapGenesToRownames(object_symbols, genes = geneIDs)
#' mapGenesToRownames(object_symbols, genes = geneNames)
#'
#' mapGenesToIDs(object, genes = geneIDs)
#' mapGenesToIDs(object, genes = geneNames)
#' mapGenesToIDs(object_symbols, genes = geneIDs)
#' mapGenesToIDs(object_symbols, genes = geneNames)
#'
#' mapGenesToSymbols(object, genes = geneIDs)
#' mapGenesToSymbols(object, genes = geneNames)
#' mapGenesToSymbols(object_symbols, genes = geneIDs)
#' mapGenesToSymbols(object_symbols, genes = geneNames)
NULL



.mapGenesError <- "Failed to map genes"



#' @rdname mapGenes
#' @export
setMethod(
    "mapGenesToRownames",
    signature("SummarizedExperiment"),
    function(object, genes) {
        validObject(object)
        assert_is_character(genes)
        assert_is_non_empty(genes)

        # Return early if the `genes` vector already matches rownames.
        if (all(genes %in% rownames(object))) {
            return(genes)
        }

        # Get the gene-to-symbol mappings.
        g2s <- gene2symbol(object)
        if (is.null(g2s)) {
            stop(.mapGenesError)
        }
        assert_are_identical(rownames(g2s), rownames(object))

        if (any(genes %in% g2s[["geneName"]])) {
            # User passed in gene names, but the object uses gene IDs.
            assert_is_subset(genes, g2s[["geneName"]])
            assertAllAreUniqueGeneNames(object, genes, severity = "warning")
            match <- match(x = genes, table = g2s[["geneName"]])
            assert_all_are_not_na(match)
            return <- rownames(g2s[match, , drop = FALSE])
        } else if (any(genes %in% g2s[["geneID"]])) {
            # User passed in gene IDs, but the object uses gene names.
            # This scenario isn't common but we're supporting it anyway.
            assert_is_subset(genes, g2s[["geneID"]])
            match <- match(x = genes, table = g2s[["geneID"]])
            assert_all_are_not_na(match)
            return <- rownames(g2s[match, , drop = FALSE])
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
    "mapGenesToIDs",
    signature("SummarizedExperiment"),
    function(object, genes) {
        validObject(object)
        assert_is_character(genes)
        assert_is_non_empty(genes)

        # Get the gene-to-symbol mappings.
        g2s <- gene2symbol(object)
        if (is.null(g2s)) {
            stop(.mapGenesError)
        }
        assert_are_identical(rownames(g2s), rownames(object))

        if (any(genes %in% g2s[["geneID"]])) {
            # User passed in gene identifiers.
            assert_is_subset(genes, g2s[["geneID"]])
            return(genes)
        } else if (any(genes %in% g2s[["geneName"]])) {
            # User passed in gene names.
            assert_is_subset(genes, g2s[["geneName"]])
            assertAllAreUniqueGeneNames(object, genes, severity = "warning")
            match <- match(x = genes, table = g2s[["geneName"]])
            assert_all_are_not_na(match)
            return <- g2s[match, "geneID", drop = TRUE]
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
    "mapGenesToSymbols",
    signature("SummarizedExperiment"),
    function(object, genes) {
        validObject(object)
        assert_is_character(genes)
        assert_is_non_empty(genes)

        # Get the gene-to-symbol mappings.
        g2s <- gene2symbol(object)
        if (is.null(g2s)) {
            stop(.mapGenesError)
        }
        assert_are_identical(rownames(g2s), rownames(object))

        if (any(genes %in% g2s[["geneID"]])) {
            # User passed in gene identifiers.
            assert_is_subset(genes, g2s[["geneID"]])
            match <- match(x = genes, table = g2s[["geneID"]])
            assert_all_are_not_na(match)
            return <- g2s[match, "geneName", drop = TRUE]
        } else if (any(genes %in% g2s[["geneName"]])) {
            # User passed in gene names.
            assert_is_subset(genes, g2s[["geneName"]])
            assertAllAreUniqueGeneNames(object, genes, severity = "warning")
            return(genes)
        } else {
            stop(.mapGenesError)
        }

        return <- as.character(return)
        names(return) <- genes
        return
    }
)
