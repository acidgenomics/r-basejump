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
#' @param object Object containing column data that defines interesting groups.
#'
#' @return Modified object, containing an `interestingGroups` column.
#' @export
#'
#' @examples
#' object <- sampleData(rse_small)
#' x <- uniteInterestingGroups(
#'     object = object,
#'     interestingGroups = c("genotype", "treatment")
#' )
#' print(x)
NULL



#' @rdname uniteInterestingGroups
#' @export
setMethod(
    f = "uniteInterestingGroups",
    signature = signature("data.frame"),
    definition = function(object, interestingGroups) {
        interestingGroups <- matchInterestingGroups(object, interestingGroups)
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
    f = "uniteInterestingGroups",
    signature = signature("DataFrame"),
    definition = getMethod("uniteInterestingGroups", "data.frame")
)




#' @rdname uniteInterestingGroups
#' @export
setMethod(
    f = "uniteInterestingGroups",
    signature = signature("tbl_df"),
    definition = function(object, interestingGroups) {
        interestingGroups <- matchInterestingGroups(object, interestingGroups)
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
