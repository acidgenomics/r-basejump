#' Sanitize Column Data
#'
#' @family Sanitization Functions
#'
#' @inheritParams general
#'
#' @return `DataFrame`.
#' @export
#'
#' @examples
#' colData <- DataFrame(
#'     row.names = c("sample_1", "sample_2", "sample_3", "sample_4"),
#'     genotype = c("wt", "ko", "wt", "ko"),
#'     batch = c(1L, 1L, 2L, 2L)
#' )
#' print(colData)
#' # Note that all columns will be sanitized to factor
#' sanitizeColData(colData)
sanitizeColData <- function(object) {
    assert_is_all_of(object, "DataFrame")
    assert_is_non_empty(object)
    assert_has_colnames(object)
    object <- camel(object)
    list <- lapply(
        X = object,
        FUN = function(x) {
            droplevels(as.factor(x))
        }
    )
    data <- as(list, "DataFrame")
    rownames(data) <- rownames(object)
    data
}
