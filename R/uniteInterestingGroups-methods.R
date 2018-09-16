#' Unite Interesting Groups
#'
#' Create a single interesting groups column ("`interestingGroups`") used for
#' coloring in plots. When multiple interesting groups are present, unite into a
#' single column, delimited by a colon.
#'
#' @name uniteInterestingGroups
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param object Object containing column data that defines interesting groups.
#'
#' @return Modified object, containing an `interestingGroups` column.
#'
#' @examples
#' from <- sampleData(rse_small)
#' to <- uniteInterestingGroups(
#'     object = from,
#'     interestingGroups = c("genotype", "treatment")
#' )
#' print(to)
NULL



.uniteInterestingGroups.df <-  # nolint
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
    definition = .uniteInterestingGroups.df
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
    definition = .uniteInterestingGroups.tbl_df
)
