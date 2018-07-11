#' Unite Interesting Groups
#'
#' Create a single interesting groups column ("`interestingGroups`") used for
#' coloring in plots. When multiple interesting groups are present, unite into a
#' single column, delimited by a colon.
#'
#' @name uniteInterestingGroups
#' @family Data Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param object Object containing interesting groups in multiple columns.
#'
#' @return Data frame, now containing an `interestingGroups` column.
#' @export
#'
#' @examples
#' x <- sampleData(rse_bcb, interestingGroups = NULL)
#' y <- uniteInterestingGroups(x, interestingGroups = c("treatment", "day"))
#' y[["interestingGroups"]]
NULL



# Methods ======================================================================
#' @rdname uniteInterestingGroups
#' @export
setMethod(
    "uniteInterestingGroups",
    signature("data.frame"),
    function(object, interestingGroups) {
        assert_is_character(interestingGroups)
        assert_is_subset(interestingGroups, colnames(object))
        # This approach will return numerics for DataFrame class, so
        # coercing columns to data.frame
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
        assert_is_character(interestingGroups)
        assert_is_subset(interestingGroups, colnames(object))
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
