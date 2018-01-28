#' Check Gene to Symbol Mapping Data
#'
#' @param object [data.frame] containing Ensembl gene identifier to gene symbol
#'   mappings. Must be structured as a two column [data.frame] with "ensgene"
#'   and "symbol" columns.
#'
#' @return Silent on pass, stop on error.
#' @export
#'
#' @examples
#' gene2symbol <- annotable("Homo sapiens", format = "gene2symbol")
#' checkGene2symbol(gene2symbol)
checkGene2symbol <- function(object) {
    if (!is.data.frame(object)) {
        abort("gene2symbol must be data.frame")
    }
    colnames <- c("ensgene", "symbol")
    if (!identical(colnames(object), colnames)) {
        abort(paste(
            "gene2symbol must contain:", toString(colnames)
        ))
    }
}
