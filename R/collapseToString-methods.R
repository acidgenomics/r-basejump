#' Collapse to String
#'
#' @name collapseToString
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param sep `string`. Separator. Defaults to comma.
#' @param unique `boolean`. Unique values.
#' @param sort `boolean`. Sort values.
#' @param removeNA `boolean`. Remove NA values.
#'
#' @return
#' - For `atomic`: `string`.
#' - For column data: Object collapsed to a single row.
#'
#' @seealso
#' - [base::toString()].
#'
#' @examples
#' # character ====
#' groceries <- c(NA, NA, "milk", "eggs", "eggs", "veggies")
#' collapseToString(
#'     groceries,
#'     unique = TRUE,
#'     sort = TRUE,
#'     removeNA = TRUE
#' )
#' collapseToString(
#'     groceries,
#'     unique = FALSE,
#'     sort = FALSE,
#'     removeNA = FALSE
#' )
#'
#' # numeric ====
#' collapseToString(seq(1:5))
#'
#' # logical ====
#' collapseToString(c(TRUE, FALSE))
#' collapseToString(c(NA, NaN))
#'
#' # data.frame ====
#' ggplot2::mpg %>%
#'     head() %>%
#'     collapseToString(sort = TRUE, unique = TRUE) %>%
#'     t()
NULL



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("atomic"),
    function(
        object,
        sep = ", ",
        sort = FALSE,
        removeNA = FALSE,
        unique = FALSE
    ) {
        assert_is_any_of(object, c("factor", "vector"))
        assert_is_a_string(sep)
        assert_is_a_bool(unique)
        assert_is_a_bool(sort)

        # Early return unmodified if scalar.
        if (is_scalar(object)) {
            return(object)
        }

        # Sort, if desired.
        if (isTRUE(sort)) {
            object <- sort(object, na.last = TRUE)
        }

        # Remove NA values, if desired.
        if (!all(is.na(object))) {
            if (isTRUE(removeNA)) {
                object <- na.omit(object)
            } else {
                object <- str_replace_na(object)
            }
        }

        # Make unique, if desired.
        if (isTRUE(unique)) {
            object <- unique(object)
        }

        object %>%
            as.character() %>%
            paste(collapse = sep)
    }
)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("matrix"),
    function(
        object,
        sep = ", ",
        sort = FALSE,
        removeNA = FALSE,
        unique = FALSE
    ) {
        # Passthrough to atomic method: sep, unique, sort.
        assert_is_non_empty(object)

        # Coerce to tibble to perform the collapse.
        collapse <- object %>%
            as(Class = "tbl_df") %>%
            mutate_all(funs(fixNA)) %>%
            summarize_all(funs(
                collapseToString(
                    object = .,
                    sep = sep,
                    sort = sort,
                    removeNA = removeNA,
                    unique = unique
                )
            ))

        # Coerce collapsed tibble back to original object class.
        as(object = collapse, Class = class(object)[[1L]])
    }
)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("data.frame"),
    getMethod("collapseToString", "matrix")
)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("DataFrame"),
    getMethod("collapseToString", "matrix")
)
