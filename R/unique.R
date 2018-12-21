#' Sort and make unique
#'
#' This is a convenience function to quickly sort and atomic vector and make the
#' values unique. The function also strips `NA` values. This is useful for
#' repetitive gene vector operations, for example.
#'
#' @export
#'
#' @param object `atomic`.
#'
#' @return `atomic`.
#'
#' @examples
#' sortUnique(c(NA, NA, "milk", "eggs", "eggs"))
sortUnique <- function(object) {
    assert(is.atomic(object))
    object %>%
        sort(na.last = TRUE) %>%
        unique()
}



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
