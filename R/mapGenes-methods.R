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



.mapGenesError <- "Failed to map genes"



# mapGenesToRownames ===========================================================
.mapGenesToRownames.gene2symbol <-  # nolint
    function(object, genes) {
        validObject(object)
        assert_is_character(genes)
        assert_is_non_empty(genes)
        if (any(genes %in% rownames(object))) {
            assert_is_subset(genes, rownames(object))
            return(genes)
        } else if (any(genes %in% object[["geneName"]])) {
            assert_is_subset(genes, object[["geneName"]])
            assertAllAreUniqueGeneNames(object, genes)
            match <- match(x = genes, table = object[["geneName"]])
            assert_all_are_not_na(match)
            return <- rownames(object[match, , drop = FALSE])
        } else if (any(genes %in% object[["geneID"]])) {
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



.mapGenesToRownames.SE <-  # nolint
    function(object, genes) {
        validObject(object)
        g2s <- gene2symbol(object)
        assert_are_identical(rownames(g2s), rownames(object))
        mapGenesToRownames(object = g2s, genes = genes)
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
    function(object, genes) {
        validObject(object)
        # object <- as(object, "DataFrame")
        assert_is_character(genes)
        assert_is_non_empty(genes)
        if (any(genes %in% rownames(object))) {
            assert_is_subset(genes, rownames(object))
            return <- as(object, "DataFrame")[genes, "geneID", drop = TRUE]
        } else if (any(genes %in% object[["geneID"]])) {
            assert_is_subset(genes, object[["geneID"]])
            return(genes)
        } else if (any(genes %in% object[["geneName"]])) {
            assert_is_subset(genes, object[["geneName"]])
            assertAllAreUniqueGeneNames(object, genes)
            match <- match(x = genes, table = object[["geneName"]])
            assert_all_are_not_na(match)
            return <- as(object, "DataFrame")[match, "geneID", drop = TRUE]
        } else {
            stop(.mapGenesError)
        }
        return <- as.character(return)
        names(return) <- genes
        return
    }



.mapGenesToIDs.SE <-  # nolint
    function(object, genes) {
        validObject(object)
        g2s <- gene2symbol(object)
        assert_are_identical(rownames(g2s), rownames(object))
        mapGenesToIDs(object = g2s, genes = genes)
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
    function(object, genes) {
        validObject(object)
        # object <- as(object, "DataFrame")
        assert_is_character(genes)
        assert_is_non_empty(genes)
        if (any(genes %in% rownames(object))) {
            assert_is_subset(genes, rownames(object))
            return <- as(object, "DataFrame")[genes, "geneName", drop = TRUE]
        } else if (any(genes %in% object[["geneID"]])) {
            assert_is_subset(genes, object[["geneID"]])
            match <- match(x = genes, table = object[["geneID"]])
            assert_all_are_not_na(match)
            return <- as(object, "DataFrame")[match, "geneName", drop = TRUE]
        } else if (any(genes %in% object[["geneName"]])) {
            assert_is_subset(genes, object[["geneName"]])
            assertAllAreUniqueGeneNames(object, genes)
            return(genes)
        } else {
            stop(.mapGenesError)
        }
        return <- as.character(return)
        names(return) <- genes
        return
    }



.mapGenesToSymbols.SE <-  # nolint
    function(object, genes) {
        validObject(object)
        g2s <- gene2symbol(object)
        assert_are_identical(rownames(g2s), rownames(object))
        mapGenesToSymbols(object = g2s, genes = genes)
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
