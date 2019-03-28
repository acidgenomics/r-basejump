#' Convert to a unique character string
#'
#' @export
#'
#' @param object `atomic`.
#'
#' @seealso `toString`.
#'
#' @return `character(1)`.
#'
#' @examples
#' toStringUnique(c("hello", "world", NA, "hello", "world", NA))
toStringUnique <- function(object) {
    assert(is.atomic(object))
    object %>%
        as.character() %>%
        na.omit() %>%
        unique() %>%
        toString()
}
