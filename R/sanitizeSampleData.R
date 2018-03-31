#' Sanitize Sample Data
#'
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `DataFrame`.
#' @export
#'
#' @examples
#' # data.frame ====
#' from <- data.frame(
#'     "genotype" = c("wt", "ko", "wt", "ko"),
#'     "batch" = c(1L, 1L, 2L, 2L),
#'     row.names = c("sample_1", "sample_2", "sample_3", "sample_4"),
#'     stringsAsFactors = FALSE
#' )
#' glimpse(from)
#' to <- sanitizeSampleData(from)
#' glimpse(to)
#'
#' # DataFrame ====
#' from <- DataFrame(
#'     "genotype" = c("wt", "ko", "wt", "ko"),
#'     "batch" = c(1L, 1L, 2L, 2L),
#'     row.names = c("sample_1", "sample_2", "sample_3", "sample_4")
#' )
#' glimpse(from)
#' to <- sanitizeSampleData(from)
#' glimpse(to)
sanitizeSampleData <- function(object) {
    assert_is_any_of(object, c("data.frame", "DataFrame"))
    assert_is_non_empty(object)
    assert_has_colnames(object)
    class <- class(object)[[1L]]
    object <- as(object, "DataFrame")
    object <- camel(object)
    list <- lapply(
        X = object,
        FUN = function(x) {
            droplevels(as.factor(x))
        }
    )
    # DataFrame class supports coercion from list; data.frame does not
    data <- as(list, "DataFrame")
    data <- as(data, class)
    rownames(data) <- rownames(object)
    data
}
