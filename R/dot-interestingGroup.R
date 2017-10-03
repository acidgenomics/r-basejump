#' Primary Interesting Group
#'
#' Constructor function.
#'
#' @family Internal Constructors
#' @keywords internal
#'
#' @inheritParams AllGenerics
#'
#' @return `string`.
#' @export
.interestingGroup <- function(object) {
    metadata(object) %>%
        .[["interestingGroups"]] %>%
        .[[1]] %>%
        as.character()
}
