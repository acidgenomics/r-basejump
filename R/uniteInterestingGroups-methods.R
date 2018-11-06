#' @name uniteInterestingGroups
#' @inheritParams params
#' @examples
#' data(rse)
#' object <- rse
#' from <- sampleData(object)
#' to <- uniteInterestingGroups(
#'     object = from,
#'     interestingGroups = interestingGroups(object)
#' )
#' print(to)
NULL



uniteInterestingGroups.DataFrame <-  # nolint
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
        names(value) <- NULL
        object[["interestingGroups"]] <- as.factor(value)
        object
    }



#' @rdname uniteInterestingGroups
#' @export
setMethod(
    f = "uniteInterestingGroups",
    signature = signature("DataFrame"),
    definition = uniteInterestingGroups.DataFrame
)
