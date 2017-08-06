#' Convert Ensembl Identifier to Entrez
#'
#' @rdname gene2entrez
#' @name gene2entrez
#' @inherit annotable
#'
#' @return Grouped [tibble].
#'
#' @examples
#' gene2entrez("grch38")
NULL



#' Methods ====
#' @rdname gene2entrez
#' @export
setMethod("gene2entrez", "character", function(object) {
    annotable(object, format = "gene2entrez")
})
