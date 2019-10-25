#' Match human gene orthologs
#'
#' @note Updated 2019-10-25.
#' @export
#'
#' @inheritParams acidroxygen::params
#'
#' @return `DataFrame`.
#'   Data frame containing mapping columns:
#'
#'   - geneID
#'   - hgncID
#'   - geneName
#'   - hgncName
#'
#' @examples
#' genes <- c(
#'     "ENSMUSG00000000001", "ENSMUSG00000000003",
#'     "ENSMUSG00000000028", "ENSMUSG00000000031",
#'     "ENSMUSG00000000037", "ENSMUSG00000000049"
#' )
#' ## Protect against Ensembl timeouts causing build checks to fail.
#' ## > if (goalie::hasInternet("https://ensembl.org")) {
#' ## >     matchHumanOrthologs(genes = genes, ensemblRelease = 87L)
#' ## > }
matchHumanOrthologs <- function(
    genes,
    organism = NULL,
    ensemblRelease = NULL
) {
    timeout <- "biomaRt timed out connecting to Ensembl."
    assert(
        isCharacter(genes),
        isString(organism, nullOK = TRUE),
        isInt(ensemblRelease, nullOK = TRUE)
    )
    if (is.null(organism)) {
        organism <- detectOrganism(genes)
    }
    ## Don't allow the user to pass in human genes, since this makes no sense.
    assert(!identical(organism, "Homo sapiens"))
    ## Match the Ensembl release to the archive host name, required for biomaRt.
    host <- sub(
        pattern = "^http(s)?://([a-z0-9.]+)/?$",
        replacement = "\\2",
        x = matchEnsemblReleaseToURL(ensemblRelease)
    )
    marts <- tryCatch(
        expr = listMarts(host = host),
        error = function(e) stop(timeout)
    )
    which <- match("ENSEMBL_MART_ENSEMBL", marts[["biomart"]])
    version <- marts[["version"]][which]
    message(sprintf(
        "Matching orthologs against %s (%s) with biomaRt %s.",
        version, host, packageVersion("biomaRt")
    ))
    ## e.g. "mmusculus_gene_ensembl".
    dataset <- paste0(
        tolower(sub(
            pattern = "^([A-Z])([a-z]+)\\s([a-z]+)$",
            replacement = "\\1\\3",
            x = organism
        )),
        "_gene_ensembl"
    )
    mart <- tryCatch(
        expr = useMart(
            ## "ensembl" also works.
            biomart = "ENSEMBL_MART_ENSEMBL",
            dataset = dataset,
            host = host
        ),
        error = function(e) stop(timeout)
    )
    map <- tryCatch(
        expr = select(
            x = mart,
            keys = genes,
            keytype = "ensembl_gene_id",
            columns =  c(
                "ensembl_gene_id",
                "hsapiens_homolog_ensembl_gene"
            )
        ),
        error = function(e) stop(timeout)
    )
    map <- as(map, "DataFrame")
    colnames(map) <- c("geneID", "hgncID")
    map <- sanitizeNA(map)
    ## Get the corresponding gene-to-symbol mappings.
    message(sprintf("Getting %s gene symbols.", organism))
    g2s <- makeGene2SymbolFromEnsembl(
        organism = organism,
        release = ensemblRelease,
        format = "unmodified"
    )
    message("Getting Homo sapiens gene symbols.")
    g2shs <- makeGene2SymbolFromEnsembl(
        organism = "Homo sapiens",
        release = ensemblRelease,
        format = "unmodified"
    )
    g2shs <- as(g2shs, "DataFrame")
    colnames(g2shs) <- c("hgncID", "hgncName")
    ## Return.
    out <- map
    out <- leftJoin(out, g2s, by = "geneID")
    out <- leftJoin(out, g2shs, by = "hgncID")
    rownames(out) <- out[["geneID"]]
    out <- out[, sort(colnames(out))]
    out
}
