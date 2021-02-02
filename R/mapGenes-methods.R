#' Map genes
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
#' by sanitizing them with `make.names`. If a user requests a gene name that
#' is duplicated, these functions will return a warning.
#'
#' @name mapGenes
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param strict `logical(1)`.
#'   Require all genes to match. Recommended by default. If set `FALSE`, instead
#'   will return a warning to the user, and subset the genes vector to only
#'   include matches.
#' @param ... Additional arguments.
#'
#' @return `character`.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#' print(rse)
#'
#' rownames <- head(rownames(rse))
#' print(rownames)
#'
#' g2s <- Gene2Symbol(rse)
#' geneIds <- head(g2s[["geneId"]])
#' print(geneIds)
#'
#' geneNames <- head(g2s[["geneName"]])
#' print(geneNames)
#'
#' ## Row names.
#' mapGenesToRownames(rse, genes = rownames)
#' mapGenesToRownames(rse, genes = geneIds)
#' mapGenesToRownames(rse, genes = geneNames)
#'
#' ## Gene identifiers.
#' mapGenesToIDs(rse, genes = rownames)
#' mapGenesToIDs(rse, genes = geneIds)
#' mapGenesToIDs(rse, genes = geneNames)
#'
#' ## Gene names (symbols).
#' mapGenesToSymbols(rse, genes = rownames)
#' mapGenesToSymbols(rse, genes = geneIds)
#' mapGenesToSymbols(rse, genes = geneNames)
NULL



## Updated 2021-02-02.
.mapGenes <- function(object, genes, strict = TRUE) {
    validObject(object)
    assert(
        is(object, "Gene2Symbol"),
        isCharacter(genes),
        isFlag(strict)
    )
    cols <- c("geneId", "geneName")
    if (!identical(cols, colnames(object))) {
        colnames(object) <- cols
    }
    validObject(object)
    ## Prepare the match table.
    if (any(genes %in% rownames(object))) {
        table <- rownames(object)
    } else if (any(genes %in% object[["geneName"]])) {
        assert(matchesUniqueGeneNames(object, genes))
        table <- object[["geneName"]]
    } else if (any(genes %in% object[["geneId"]])) {
        table <- object[["geneId"]]
    } else {
        stop(sprintf(
            "All genes failed to map: %s.",
            toString(genes, width = 100L)
        ))
    }
    ## Match the user input `genes` vector to the table.
    match <- match(x = genes, table = table)
    names(match) <- genes
    ## Stop or warn if there are unmapped genes.
    if (isTRUE(strict)) {
        fun <- stop
    } else {
        fun <- message
    }
    unmapped <- which(is.na(match))
    if (length(unmapped) > 0L) {
        fun(sprintf(
            "Some genes failed to map: %s.",
            toString(genes[unmapped], width = 200L)
        ))
    }
    ## Return the identifiers that map to rownames.
    mapped <- na.omit(match)
    assert(hasLength(mapped))
    mapped
}



## mapGenesToRownames ==========================================================
## Updated 2021-02-02.
`mapGenesToRownames,Gene2Symbol` <-  # nolint
    function(object, genes, strict = TRUE) {
        mapped <- do.call(
            what = .mapGenes,
            args = list(
                "object" = object,
                "genes" = genes,
                "strict" = strict
            )
        )
        return <- rownames(object[mapped, , drop = FALSE])
        return <- as.character(return)
        names(return) <- names(mapped)
        return
    }



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToRownames",
    signature = signature("Gene2Symbol"),
    definition = `mapGenesToRownames,Gene2Symbol`
)



## Updated 2021-02-02.
`mapGenesToRownames,SummarizedExperiment` <-  # nolint
    function(object, genes, strict = TRUE) {
        validObject(object)
        assert(isFlag(strict))
        ## Check to see if object contains gene-to-symbol mappings.
        g2s <- tryCatch(
            expr = {
                suppressMessages({
                    g2s <- Gene2Symbol(object)
                })
            },
            error = function(e) {
                NULL
            }
        )
        if (is(g2s, "Gene2Symbol")) {
            assert(identical(rownames(g2s), rownames(object)))
            do.call(
                what = mapGenesToRownames,
                args = list(
                    "object" = g2s,
                    "genes" = genes,
                    "strict" = strict
                )
            )
        } else {
            ## Match the user input `genes` vector to the table.
            table <- rownames(object)
            match <- match(x = genes, table = table)
            names(match) <- genes
            ## Stop or warn if there are unmapped genes.
            if (isTRUE(strict)) {
                fun <- stop
            } else {
                fun <- warning
            }
            unmapped <- which(is.na(match))
            if (length(unmapped) > 0L) {
                fun(sprintf(
                    "Some genes failed to map: %s.",
                    toString(genes[unmapped], width = 100L)
                ))
            }
            ## Return the identifiers that map to rownames.
            mapped <- na.omit(match)
            assert(hasLength(mapped))
            genes[mapped]
        }
    }



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToRownames",
    signature = signature("SummarizedExperiment"),
    definition = `mapGenesToRownames,SummarizedExperiment`
)



## mapGenesToIDs ===============================================================
## Updated 2021-02-02.
`mapGenesToIDs,Gene2Symbol` <-  # nolint
    function(object, genes, strict = TRUE) {
        mapped <- do.call(
            what = .mapGenes,
            args = list(
                "object" = object,
                "genes" = genes,
                "strict" = strict
            )
        )
        return <- object[mapped, , drop = FALSE]
        return <- return[["geneId"]]
        return <- as.character(return)
        names(return) <- names(mapped)
        return
    }



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToIDs",
    signature = signature("Gene2Symbol"),
    definition = `mapGenesToIDs,Gene2Symbol`
)



## Updated 2021-02-02.
`mapGenesToIDs,SummarizedExperiment` <-  # nolint
    function(object, genes, strict = TRUE) {
        validObject(object)
        suppressMessages({
            g2s <- Gene2Symbol(object)
        })
        assert(identical(rownames(g2s), rownames(object)))
        do.call(
            what = mapGenesToIDs,
            args = list(
                "object" = g2s,
                "genes" = genes,
                "strict" = strict
            )
        )
    }



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToIDs",
    signature = signature("SummarizedExperiment"),
    definition = `mapGenesToIDs,SummarizedExperiment`
)



## mapGenesToSymbols ===========================================================
## Updated 2021-02-02.
`mapGenesToSymbols,Gene2Symbol` <-  # nolint
    function(object, genes, strict = TRUE) {
        mapped <- do.call(
            what = .mapGenes,
            args = list(
                "object" = object,
                "genes" = genes,
                "strict" = strict
            )
        )
        return <- object[mapped, , drop = FALSE]
        return <- return[["geneName"]]
        return <- as.character(return)
        names(return) <- names(mapped)
        return
    }



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToSymbols",
    signature = signature("Gene2Symbol"),
    definition = `mapGenesToSymbols,Gene2Symbol`
)



## Updated 2021-02-02.
`mapGenesToSymbols,SummarizedExperiment` <-  # nolint
    function(object, genes, strict = TRUE) {
        validObject(object)
        suppressMessages({
            g2s <- Gene2Symbol(object)
        })
        assert(identical(rownames(g2s), rownames(object)))
        do.call(
            what = mapGenesToSymbols,
            args = list(
                "object" = g2s,
                "genes" = genes,
                "strict" = strict
            )
        )
    }



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToSymbols",
    signature = signature("SummarizedExperiment"),
    definition = `mapGenesToSymbols,SummarizedExperiment`
)
