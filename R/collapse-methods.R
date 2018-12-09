# collapseToString =============================================================
#' Collapse to String
#'
#' @name collapseToString
#' @inheritParams params
#'
#' @param sep `string`. Separator. Defaults to comma.
#' @param unique `boolean`. Unique values.
#' @param sort `boolean`. Sort values.
#' @param removeNA `boolean`. Remove NA values.
#'
#' @seealso `toString()`.
#'
#' @return
#' - `atomic`: `string`.
#' - `dim()`: Object of same class, collapsed to a single row.
#'
#' @examples
#' ## character ====
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
#' ## numeric ====
#' collapseToString(seq(1:5))
#'
#' ## logical ====
#' collapseToString(c(TRUE, FALSE))
#' collapseToString(c(NA, NaN))
#'
#' ## data.frame ====
#' datasets::iris %>%
#'     head() %>%
#'     collapseToString(sort = TRUE, unique = TRUE) %>%
#'     t()
NULL



collapseToString.atomic <-  # nolint
    function(
        object,
        sep = ", ",
        sort = FALSE,
        removeNA = FALSE,
        unique = FALSE
    ) {
        assert_is_any_of(
            x = object,
            classes = c("character", "factor", "vector")
        )
        assertString(sep)
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
                object <- removeNA(object)
            } else {
                object <- sanitizeNA(object)
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



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("atomic"),
    definition = collapseToString.atomic
)



collapseToString.matrix <-  # nolint
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
            as.data.frame() %>%
            as_tibble(rownames = NULL) %>%
            mutate_all(funs(sanitizeNA)) %>%
            summarise_all(funs(
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



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("matrix"),
    definition = collapseToString.matrix
)



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("data.frame"),
    definition = collapseToString.matrix
)



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("DataFrame"),
    definition = collapseToString.matrix
)



# sortUnique ===================================================================
#' Sort and Make Unique
#'
#' This is a convenience function to quickly sort and atomic vector and make the
#' values unique. The function also strips `NA` values. This is useful for
#' repetitive gene vector operations, for example.
#'
#' @export
#'
#' @param object `atomic`.
#'
#' @return `atomic`.
#'
#' @examples
#' sortUnique(c(NA, NA, "milk", "eggs", "eggs"))
sortUnique <- function(object) {
    assert_is_atomic(object)
    object %>%
        sort(na.last = TRUE) %>%
        unique()
}



# toStringUnique ===============================================================
#' Convert to a Unique Character String
#'
#' @export
#'
#' @param object `atomic`.
#'
#' @seealso `toString()`.
#'
#' @return `string`.
#'
#' @examples
#' toStringUnique(c("hello", "world", NA, "hello", "world", NA))
toStringUnique <- function(object) {
    assert_is_atomic(object)
    object %>%
        as.character() %>%
        na.omit() %>%
        unique() %>%
        toString()
}
