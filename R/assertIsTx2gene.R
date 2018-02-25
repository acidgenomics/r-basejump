#' Check Transcript to Gene Mapping Data
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param x [data.frame] containing Ensembl transcript to gene identifier
#'   mappings. Must be structured as a two column [data.frame] with "enstxp" and
#'   "ensgene" columns.
#'
#' @export
#'
#' @examples
#' # Success
#' tx2gene <- tx2gene("Homo sapiens")
#' assertIsTx2gene(tx2gene)
#'
#' # Failure
#' tryCatch(
#'     assertIsTx2gene(mtcars),
#'     error = function(e) e)
assertIsTx2gene <- function(x, severity = "stop") {
    assert_is_data.frame(x, severity = severity)
    assert_are_identical(
        x = colnames(x),
        y = c("enstxp", "ensgene"),
        severity = severity)
}
