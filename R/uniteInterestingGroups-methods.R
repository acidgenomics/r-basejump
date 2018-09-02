#' Unite Interesting Groups
#'
#' Create a single interesting groups column ("`interestingGroups`") used for
#' coloring in plots. When multiple interesting groups are present, unite into a
#' single column, delimited by a colon.
#'
#' @name uniteInterestingGroups
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param object Object containing multiple interesting groups.
#'
#' @return Data frame containing an `interestingGroups` column.
#' @export
#'
#' @examples
#' x <- sampleData(rse_small)
#' y <- uniteInterestingGroups(x, interestingGroups = c("treatment", "day"))
#' y[["interestingGroups"]]
NULL



.returnInterestingGroups <- function(data, interestingGroups) {
    # If `interestingGroups` isn't defined, use sample name by default.
    if (is.null(interestingGroups)) {
        interestingGroups <- "sampleName"
    }
    assert_is_character(interestingGroups)
    assert_is_subset(interestingGroups, colnames(data))
    interestingGroups
}



#' @rdname uniteInterestingGroups
#' @export
setMethod(
    "uniteInterestingGroups",
    signature("data.frame"),
    function(object, interestingGroups) {
        interestingGroups <- .returnInterestingGroups(object, interestingGroups)
        # This approach will return numerics for `DataFrame` class, so
        # coercing columns to data.frame.
        data <- as.data.frame(object[, interestingGroups, drop = FALSE])
        value <- apply(
            X = data,
            MARGIN = 1L,
            FUN = paste,
            collapse = ":"
        )
        object[["interestingGroups"]] <- as.factor(value)
        object
    }
)



#' @rdname uniteInterestingGroups
#' @export
setMethod(
    "uniteInterestingGroups",
    signature("DataFrame"),
    getMethod("uniteInterestingGroups", "data.frame")
)




#' @rdname uniteInterestingGroups
#' @export
setMethod(
    "uniteInterestingGroups",
    signature("tbl_df"),
    function(object, interestingGroups) {
        interestingGroups <- .returnInterestingGroups(object, interestingGroups)
        object[["interestingGroups"]] <- NULL
        object <- unite(
            data = object,
            col = interestingGroups,
            !!interestingGroups,
            sep = ":",
            remove = FALSE
        )
        object[["interestingGroups"]] <-
            as.factor(object[["interestingGroups"]])
        object
    }
)
