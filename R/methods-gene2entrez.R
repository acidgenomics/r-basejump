#' Convert Ensembl Identifier to Entrez
#'
#' @rdname gene2entrez
#' @inherit annotable
#'
#' @return Grouped [tibble].
#' @export
#'
#' @examples
#' gene2entrez("grcm38")
setMethod("gene2entrez", "character", function(object) {
    annotable(object, format = "gene2entrez")
})
