#' Annotation Column Formal Assert Check
#'
#' @family Assert Checks
#'
#' @inherit assert
#' @inheritParams general
#'
#' @param colData Column data.
#'
#' @export
assert_formal_annotation_col <- function(object, colData) {
    assert_has_dims(object)
    assert_is_any_of(colData, c("data.frame", "logical", "NULL"))
    if (is.data.frame(colData)) {
        assert_has_colnames(colData)
        assert_has_rownames(colData)
        assert_are_identical(colnames(object), rownames(colData))
        lapply(colData, assert_is_factor)
    }
    if (is.logical(colData)) {
        assert_is_identical_to_na(colData)
    }
}
