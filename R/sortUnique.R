#' Quickly Perform Sort Unique on a Vector
#'
#' The function also strips `NA` values. This is useful for gene list server
#' queries, for example.
#'
#' @family Atomic Vector Functions
#'
#' @inheritParams general
#'
#' @return Unique vector.
#' @export
#'
#' @examples
#' sortUnique(c(NA, NA, "milk", "eggs", "eggs"))
sortUnique <- function(object) {
    assert_is_atomic(object)
    object %>%
        sort(na.last = TRUE) %>%
        unique()
}
