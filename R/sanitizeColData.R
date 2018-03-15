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
    assert_is_any_of(object, c("data.frame", "DataFrame"))
    assert_is_non_empty(object)
    assert_has_colnames(object)
    class <- class(object)[[1L]]
    object <- camel(object)
    list <- lapply(
        X = object,
        FUN = function(x) {
            droplevels(as.factor(x))
        }
    )
    data <- do.call(cbind, list)
    data <- as(data, class)
    rownames(data) <- rownames(object)
    data
}
