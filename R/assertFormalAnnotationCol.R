#' Assert Formal Annotation Column
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param colData Column data.
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'     "sample_1" = c(1L, 2L),
#'     "sample_2" = c(3L, 4L),
#'     row.names = c("gene_1", "gene_2"),
#'     stringsAsFactors = FALSE
#' )
#' colData <- data.frame(
#'     "genotype" = c("wt", "ko"),
#'     row.names = c("sample_1", "sample_2"),
#'     stringsAsFactors = TRUE
#' )
#'
#' # Success
#' assertFormalAnnotationCol(x, colData)
#'
#' # Failure
#' tryCatch(
#'     assertFormalAnnotationCol(mtcars, colData),
#'     error = function(e) e
#' )
assertFormalAnnotationCol <- function(x, colData, severity = "stop") {
    assert_has_dims(x, severity = severity)
    assert_is_any_of(
        x = colData,
        classes = c("data.frame", "DataFrame", "logical", "NULL"),
        severity = severity
    )
    if (is.data.frame(colData)) {
        assert_has_colnames(colData, severity = severity)
        assertHasRownames(colData, severity = severity)
        assert_are_identical(
            x = colnames(x),
            y = rownames(colData),
            severity = severity
        )
        lapply(colData, assert_is_factor)
    }
    if (is.logical(colData)) {
        assert_is_identical_to_na(colData, severity = severity)
    }
}
