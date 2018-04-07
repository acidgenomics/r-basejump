#' Collapse to String
#'
#' @name collapseToString
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param sep Separator. Defaults to comma.
#' @param unique Unique values.
#' @param sort Sort values.
#' @param removeNA Remove NA values.
#'
#' @return
#' - For `atomic` vector: `character` string.
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
#' # Objects supporting `dim` function similarly
#' ggplot2::mpg %>%
#'     head() %>%
#'     collapseToString(sort = TRUE, unique = TRUE) %>%
#'     t()
NULL



# Constructors =================================================================
.collapseToString <- function(
    object,
    sep = ", ",
    sort = FALSE,
    removeNA = FALSE,
    unique = FALSE
) {
    assert_is_any_of(object, c("factor", "vector"))
    # Early return unmodified if scalar
    if (is_scalar(object)) {
        return(object)
    }
    assert_is_a_string(sep)
    assert_is_a_bool(unique)
    assert_is_a_bool(sort)

    # Sort, if desired
    if (isTRUE(sort)) {
        object <- sort(object, na.last = TRUE)
    }

    # Remove NA values, if desired
    if (!all(is.na(object))) {
        if (isTRUE(removeNA)) {
            object <- na.omit(object)
        } else {
            object <- str_replace_na(object)
        }
    }

    # Make unique, if desired
    if (isTRUE(unique)) {
        object <- unique(object)
    }

    object %>%
        as.character() %>%
        paste(collapse = sep)
}



.collapseToString.dim <- function(  # nolint
    object,
    sep = ", ",
    sort = FALSE,
    removeNA = FALSE,
    unique = FALSE
) {
    # Passthrough: sep, unique, sort
    assert_has_dims(object)

    # Stash original class and coerce to data.frame, if necessary
    if (!is.data.frame(object)) {
        class <- class(object)[[1L]]
        object <- as.data.frame(object)
    } else {
        class <- NULL
    }

    collapse <- object %>%
        mutate_all(funs(fixNA)) %>%
        summarize_all(funs(
            .collapseToString(
                object = .,
                sep = sep,
                sort = sort,
                removeNA = removeNA,
                unique = unique
            )
        ))

    if (!is.null(class)) {
        as(collapse, class)
    } else {
        collapse
    }
}



# Methods ======================================================================
#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("atomic"),
    .collapseToString
)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("data.frame"),
    .collapseToString.dim
)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("DataFrame"),
    .collapseToString.dim
)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("matrix"),
    .collapseToString.dim
)
