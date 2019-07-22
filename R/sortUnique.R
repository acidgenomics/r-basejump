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

## Updated 2019-07-22.
sortUnique <- function(object) {
    assert(is.atomic(object))
    unique(sort(object, na.last = TRUE))
}
