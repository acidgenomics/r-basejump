#' Quickly Perform Sort Unique on a Vector
#'
#' The function also strips `NA` values. This is useful for gene list server
#' queries, for example.
#'
#' @family Cleanup Utilities
#'
#' @inheritParams general
#'
#' @return Unique vector.
#' @export
#'
#' @examples
#' sortUnique(c(NA, NA, "milk", "eggs", "eggs"))
sortUnique <- function(object) {
    assert_is_any_of(object, c("factor", "vector"))
    object %>%
        sort(na.last = TRUE) %>%
        unique()
}
