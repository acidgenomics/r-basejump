#' Map protein identifiers to genes
#'
#' @name makeProtein2Gene
#' @note Updated 2020-09-25.
#'
#' @inheritParams acidroxygen::params
#' @param ids `character`.
#'   Ensembl protein identifiers.
#'   Human proteins are prefixed with "ENSP", for example.
#'
#' @examples
#' ids <- c("ENSP00000238714", "ENSP00000338157")
#' makeProtein2GeneFromEnsembl(ids)
NULL



#' @rdname makeProtein2Gene
#' @export
makeProtein2GeneFromEnsembl <- function(
    ids,
    organism = NULL,
    genomeBuild = NULL,
    release = NULL
) {
    cli_alert("Making {.var Protein2Gene} from Ensembl.")
    if (is.null(organism)) {
        organism <- detectOrganism(ids)
    }
    edb <- getEnsDb(
        organism = organism,
        genomeBuild = genomeBuild,
        release = release
    )
    df <- ensembldb::select(
        x = edb,
        keys = ids,
        keytype = "PROTEINID",
        columns = c("GENEID", "GENENAME")
    )
    df <- as(df, "DataFrame")
    df <- camelCase(df)
    colnames(df) <- gsub("id$", "ID", colnames(df))
    colnames(df) <- gsub("name$", "Name", colnames(df))
    assert(isSubset(ids, df[["proteinID"]]))
    metadata(df) <- .getEnsDbMetadata(edb)
    new(Class = "Protein2Gene", df)
}
