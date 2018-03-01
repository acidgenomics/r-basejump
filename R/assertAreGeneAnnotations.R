# TODO Add support for GRanges

#' Assert Are Ensembl Gene Annotations
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param x [data.frame] containing Ensembl gene annotations returned
#'   from the [annotable()] function.
#'
#' @export
#'
#' @examples
#' # Success
#' annotable <- annotable("Homo sapiens")
#' assertIsAnnotable(annotable)
#'
#' # Failure
#' tryCatch(
#'     assertIsAnnotable(mtcars),
#'     error = function(e) e)
assertAreGeneAnnotations <- function(x, severity = "stop") {
    assert_is_data.frame(x, severity = severity)
    assert_is_subset(
        x = c("ensgene", "symbol", "description", "biotype", "broadClass"),
        y = colnames(x),
        severity = severity)
    assert_has_rows(x, severity = severity)
}
