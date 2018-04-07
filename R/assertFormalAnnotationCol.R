#' Assert Formal Annotation Column
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
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
#' assertFormalAnnotationCol(x, colData)
assertFormalAnnotationCol <- function(
    x,
    colData,
    severity = getOption("assertive.severity", "stop")
) {
    assert_has_dimnames(x, severity = severity)
    assert_is_any_of(
        x = colData,
        classes = c("data.frame", "DataFrame", "logical", "NULL"),
        severity = severity
    )
    if (has_dims(colData)) {
        assert_has_dimnames(colData, severity = severity)
        assert_are_identical(
            x = colnames(x),
            y = rownames(colData),
            severity = severity
        )
        # All columns must be factors
        lapply(colData, function(x) {
            assert_is_factor(x, severity = severity)
        })
    }
    if (is.logical(colData)) {
        assert_is_identical_to_na(colData, severity = severity)
    }
}
