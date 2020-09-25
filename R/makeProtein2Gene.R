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
    df
    ## FIXME NEED TO OUTPUT AS PROTEIN2GENE CLASS.
    ##
    ## FIXME NEED TO SUPORT ARGUMENTS HERE.
    metadata(data) <- .slotGenomeMetadata(object)
    metadata(data)[["format"]] <- format
    new(Class = "Gene2Symbol", data)
}
