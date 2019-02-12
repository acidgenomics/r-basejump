#' @name uniteInterestingGroups
#' @inherit bioverbs::uniteInterestingGroups
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



#' @importFrom bioverbs uniteInterestingGroups
#' @aliases NULL
#' @export
bioverbs::uniteInterestingGroups



uniteInterestingGroups.DataFrame <-  # nolint
    function(object, interestingGroups) {
        assert(
            isCharacter(interestingGroups),
            isSubset(interestingGroups, colnames(object))
        )
        # Subset to get only the columns of interest.
        data <- object[, interestingGroups, drop = FALSE]
        assert(hasLength(data))
        # This approach will return numerics for `DataFrame` class, so
        # coercing columns to data.frame.
        value <- apply(
            X = as.data.frame(data),
            MARGIN = 1L,
            FUN = paste,
            collapse = ":"
        )
        value <- as.factor(value)
        assert(identical(rownames(object), names(value)))
        # Here we're using `uname()` to unname the factor, since `DataFrame`
        # stores this metadata internally differently than standard data.frame.
        # Otherwise, unit tests can return this error:
        # Attributes: <
        #   Component "listData":
        #   Component "interestingGroups":
        #   names for target but not for current
        # >
        object[["interestingGroups"]] <- unname(value)
        object
    }



#' @rdname uniteInterestingGroups
#' @export
setMethod(
    f = "uniteInterestingGroups",
    signature = signature("DataFrame"),
    definition = uniteInterestingGroups.DataFrame
)
