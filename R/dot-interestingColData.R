#' Interesting Column Data
#'
#' @family Exported Constructor Functions
#' @keywords internal
#'
#' @inheritParams AllGenerics
#'
#' @return [data.frame].
#' @export
.interestingColData <- function(object) {
    cols <- unique(c("sampleName", .interestingGroups(object)))
    colData(object) %>%
        as.data.frame() %>%
        .[, cols, drop = FALSE]
}
