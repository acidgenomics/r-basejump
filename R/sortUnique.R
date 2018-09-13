#' Sort and Make Unique
#'
#' This is a convenience function to quickly sort and atomic vector and make the
#' values unique. The function also strips `NA` values. This is useful for
#' repetitive gene vector operations, for example.
#'
#' @family Atomic Vector Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @param object `atomic`.
#'
#' @return `atomic`.
#'
#' @examples
#' sortUnique(c(NA, NA, "milk", "eggs", "eggs"))
sortUnique <- function(object) {
    assert_is_atomic(object)
    object %>%
        sort(na.last = TRUE) %>%
        unique()
}
