#' Interesting Groups
#'
#' @family Exported Constructor Functions
#' @keywords internal
#'
#' @inheritParams AllGenerics
#'
#' @return Character vector.
#' @export
.interestingGroups <- function(object) {
    metadata(object)[["interestingGroups"]]
}
