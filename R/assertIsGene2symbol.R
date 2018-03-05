#' Assert Is Gene to Symbol Mapping Data Frame
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param x `data.frame` containing Ensembl gene identifier to gene symbol
#'   mappings. Must be structured as a two column `data.frame` with "ensgene"
#'   and "symbol" columns.
#'
#' @export
#'
#' @examples
#' # Success
#' gene2symbol <- gene2symbol("Homo sapiens")
#' assertIsGene2symbol(gene2symbol)
#'
#' # Failure
#' tryCatch(
#'     assertIsGene2symbol(mtcars),
#'     error = function(e) e
#' )
assertIsGene2symbol <- function(x, severity = "stop") {
    assert_is_data.frame(x, severity = severity)
    assert_are_identical(
        x = colnames(x),
        y = c("ensgene", "symbol"),
        severity = severity
    )
    assert_has_rows(x, severity = severity)
}
