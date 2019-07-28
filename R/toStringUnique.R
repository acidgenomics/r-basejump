#' Convert to a unique character string
#'
#' @note Updated 2019-07-28.
#' @export
#'
#' @param object `atomic`.
#'
#' @seealso [`toString()`][base::toString].
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
