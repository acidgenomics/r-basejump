#' Primary Interesting Group
#'
#' @family Exported Constructor Functions
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
