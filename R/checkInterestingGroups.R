#' Check Interesting Groups
#'
#' Prevent unwanted downstream behavior when a missing interesting group
#' is requested by the user.
#'
#' @param object Object supporting [colnames()], typically a [data.frame].
#' @param interestingGroups Interesting groups character vector.
#' @param warnOnNULL Warn the user on `NULL` argument.
#'
#' @return Valid character of defined interesting groups. Stop on failure.
#' @export
checkInterestingGroups <- function(
    object,
    interestingGroups,
    warnOnNULL = FALSE) {
    if (!all(interestingGroups %in% colnames(object))) {
        stop(paste(
            "Interesting groups not defined in metadata:",
            toString(setdiff(interestingGroups, colnames(object)))
        ), call. = FALSE)
    }
    # Default to `sampleName` if `NULL`
    if (is.null(interestingGroups)) {
        if (isTRUE(warnOnNULL)) {
            warning(paste(
                "'interestingGroups' is 'NULL'.",
                "Defaulting to 'sampleName'."
            ), call. = FALSE)
        }
        interestingGroups <- "sampleName"
    }
    interestingGroups
}
