#' Unite Interesting Groups
#'
#' Create a single interesting groups column (`interestingGroups`) used for
#' coloring in plots. When multiple interesting groups are present, unite into a
#' single column, delimited by ` : `.
#'
#' @importFrom tidyr unite
#' @importFrom rlang !!! syms
#'
#' @param object Object (e.g. [data.frame]) containing interesting groups
#'   columns.
#' @param interestingGroups Character vector of interesting groups.
#'
#' @return [data.frame].
#' @export
uniteInterestingGroups <- function(object, interestingGroups) {
    # Set up the interesting groups column
    interestingGroups <- checkInterestingGroups(object, interestingGroups)
    object[["interestingGroups"]] <- NULL
    if (length(interestingGroups) > 1L) {
        object <- unite(
            data = object,
            col = interestingGroups,
            !!!syms(interestingGroups),
            sep = ":",
            remove = FALSE)
    } else {
        object[["interestingGroups"]] <- object[[interestingGroups]]
    }
    # Set the `interestingGroups` column as factor
    object[["interestingGroups"]] <- as.factor(object[["interestingGroups"]])
    object
}
