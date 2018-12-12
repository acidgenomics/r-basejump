#' Check Interesting Groups Value Matching
#'
#' Prevent unwanted downstream behavior when a missing interesting group
#' is requested by the user.
#'
#' @name matchesInterestingGroups
#' @inherit params
#'
#' @param x S4 class x.
#' @param interestingGroups `character`. Interesting groups.
#'
#' @examples
#' data(rse)
#'
#' ## Checks that columns are defined in `colData`.
#' matchesInterestingGroups(rse, "condition")
#'
#' ## Currently allowing `NULL` to pass.
#' matchesInterestingGroups(rse, NULL)
NULL



.matchesInterestingGroups <- function(x, interestingGroups) {
    ok <- isS4(x)
    if (!isTRUE(ok)) {
        return("x must be S4 class.")
    }

    # Early return on `NULL` interesting groups (e.g. example DESeqDataSet).
    # Consider returning FALSE or warning in a future update?
    if (is.null(interestingGroups)) {
        return(TRUE)
    }

    # Otherwise, require that `interestingGroups` is a character.
    ok <- isCharacter(interestingGroups)
    if (!isTRUE(ok)) {
        return("interestingGroups must contain non-empty character.")
    }

    data <- sampleData(x)

    # Check intersection with sample data.
    ok <- isSubset(interestingGroups, colnames(data))
    if (!isTRUE(ok)) {
        return("Interesting groups are not defined in sampleData().")
    }

    # Check that interesting groups columns are factors.
    ok <- all(vapply(
        X = data[, interestingGroups, drop = FALSE],
        FUN = is.factor,
        FUN.VALUE = logical(1L)
    ))
    if (!isTRUE(ok)) {
        return("Interesting group columns are not all factor.")
    }

    TRUE
}



#' @rdname matchesInterestingGroups
#' @export
matchesInterestingGroups <- makeTestFunction(.matchesInterestingGroups)
