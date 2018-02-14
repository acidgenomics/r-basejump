#' Check Gene to Symbol Mapping Data
#'
#' @family Assert Checks
#'
#' @inherit assert
#' @inheritParams general
#'
#' @param object [data.frame] containing Ensembl gene identifier to gene symbol
#'   mappings. Must be structured as a two column [data.frame] with "ensgene"
#'   and "symbol" columns.
#'
#' @export
#'
#' @examples
#' gene2symbol <- annotable("Homo sapiens", format = "gene2symbol")
#' assert_is_gene2symbol(gene2symbol)
assert_is_gene2symbol <- function(object) {  # nolint
    assert_is_data.frame(object)
    assert_are_identical(
        colnames(object),
        c("ensgene", "symbol")
    )
}
