#' Match human gene orthologs
#'
#' @note Updated 2019-08-11.
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
#' matchHumanOrthologs(genes, ensemblRelease = 87L)
matchHumanOrthologs <- function(
    genes,
    organism = NULL,
    ensemblRelease = NULL
) {
    requireNamespace("biomaRt", quietly = TRUE)
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
        expr = biomaRt::listMarts(host = host),
        error = function(e) {
            stop("biomaRt timed out connecting to Ensembl.")
        }
    )
    version <- marts[["version"]][
        match("ENSEMBL_MART_ENSEMBL", marts[["biomart"]])
    ]
    message(sprintf(
        "Matching orthologs against %s (%s) with biomart %s.",
        version, host, packageVersion("biomaRt")
    ))

    ## e.g. "mmusculus_gene_ensembl"
    dataset <- paste0(
        tolower(sub(
            pattern = "^([A-Z])([a-z]+)\\s([a-z]+)$",
            replacement = "\\1\\3",
            x = organism
        )),
        "_gene_ensembl"
    )

    mart <- tryCatch(
        expr = biomaRt::useMart(
            ## "ensembl" also works.
            biomart = "ENSEMBL_MART_ENSEMBL",
            dataset = dataset,
            host = host
        ),
        error = function(e) {
            stop("biomaRt timed out connecting to Ensembl.")
        }
    )

    map <- tryCatch(
        expr = biomaRt::select(
            x = mart,
            keys = genes,
            keytype = "ensembl_gene_id",
            columns =  c(
                "ensembl_gene_id",
                "hsapiens_homolog_ensembl_gene"
            )
        ),
        error = function(e) {
            stop("biomaRt timed out connecting to Ensembl.")
        }
    ) %>%
        as_tibble(rownames = NULL) %>%
        sanitizeNA() %>%
        set_colnames(c("geneID", "hgncID"))

    message(paste("Getting", organism, "gene symbols."))
    g2s <-
        makeGene2SymbolFromEnsembl(
            organism = organism,
            release = ensemblRelease,
            format = "unmodified"
        ) %>%
        as_tibble(rownames = NULL)

    message("Getting Homo sapiens gene symbols.")
    g2shs <-
        makeGene2SymbolFromEnsembl(
            organism = "Homo sapiens",
            release = ensemblRelease,
            format = "unmodified"
        ) %>%
        as_tibble(rownames = NULL) %>%
        set_colnames(c("hgncID", "hgncName"))

    map %>%
        left_join(g2s, by = "geneID") %>%
        left_join(g2shs, by = "hgncID") %>%
        as("DataFrame") %>%
        set_rownames(.[["geneID"]])
}
