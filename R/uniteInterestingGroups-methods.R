#' Unite Interesting Groups
#'
#' Create a single interesting groups column ("`interestingGroups`") used for
#' coloring in plots. When multiple interesting groups are present, unite into a
#' single column, delimited by a colon.
#'
#' @name uniteInterestingGroups
#' @family Metadata Functions
#' @export
#'
#' @inheritParams general
#' @param object Object containing column data that defines interesting groups.
#'
#' @return Modified object, containing an `interestingGroups` column.
#'
#' @examples
#' data(rse_small)
#' object <- rse_small
#' from <- sampleData(object)
#' to <- uniteInterestingGroups(
#'     object = from,
#'     interestingGroups = interestingGroups(object)
#' )
#' print(to)
NULL



.uniteInterestingGroups.data.frame <-  # nolint
    function(object, interestingGroups) {
        assert_is_character(interestingGroups)
        assert_is_subset(interestingGroups, colnames(object))
        # Subset to get only the columns of interest.
        data <- object[, interestingGroups, drop = FALSE]
        assert_is_non_empty(data)
        # This approach will return numerics for `DataFrame` class, so
        # coercing columns to data.frame.
        value <- apply(
            X = as.data.frame(data),
            MARGIN = 1L,
            FUN = paste,
            collapse = ":"
        )
        object[["interestingGroups"]] <- as.factor(value)
        object
    }



.uniteInterestingGroups.tbl_df <-  # nolint
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



#' @rdname uniteInterestingGroups
#' @export
setMethod(
    f = "uniteInterestingGroups",
    signature = signature("data.frame"),
    definition = .uniteInterestingGroups.data.frame
)



#' @rdname uniteInterestingGroups
#' @export
setMethod(
    f = "uniteInterestingGroups",
    signature = signature("DataFrame"),
    definition = getMethod(
        f = "uniteInterestingGroups",
        signature = signature("data.frame")
    )
)




#' @rdname uniteInterestingGroups
#' @export
setMethod(
    f = "uniteInterestingGroups",
    signature = signature("tbl_df"),
    definition = .uniteInterestingGroups.tbl_df
)
