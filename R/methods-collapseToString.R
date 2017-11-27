#' Collapse to String
#'
#' @rdname collapseToString
#' @name collapseToString
#' @family Data Manipulation Utilities
#'
#' @inheritParams AllGenerics
#'
#' @param sep Separator. Defaults to comma.
#' @param unique Unique values.
#' @param sort Sort values.
#'
#' @return
#' - For vector: String.
#' - For column data: Single row.
#'
#' @seealso
#' - [base::toString()].
#'
#' @examples
#' # character
#' groceries <- c("milk", "eggs", "eggs", "veggies", NA)
#' collapseToString(groceries, unique = TRUE, sort = FALSE)
#' collapseToString(groceries, unique = FALSE, sort = FALSE)
#'
#' # numeric
#' collapseToString(seq(1:5))
#'
#' # logical
#' collapseToString(c(TRUE, FALSE))
#' collapseToString(c(NA, NaN))
#'
#' # data.frame
#' # Objects supporting `dim` function similarly
#' mtcars %>%
#'     head() %>%
#'     collapseToString() %>%
#'     t()
NULL



# Constructors ====
#' @importFrom stats na.omit
#' @importFrom stringr str_replace_na
.collapseToString <- function(
    object,
    sep = ", ",
    unique = TRUE,
    sort = TRUE,
    ...) {
    if (length(object) > 1) {
        if (isTRUE(unique)) {
            if (!all(is.na(object))) {
                object <- na.omit(object)
            }
            object <- unique(object)
        } else {
            if (all(is.na(object))) {
                object <- str_replace_na(object)
            }
        }
    }
    if (isTRUE(sort)) {
        object <- sort(object, na.last = TRUE)
    }
    object %>%
        as.character() %>%
        paste(collapse = sep)
}



#' @importFrom dplyr funs mutate_all summarize_all
.collapseRows <- function(
    object,
    sep = ", ",
    unique = TRUE,
    sort = TRUE,
    ...) {
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
                unique = unique,
                sort = sort)
        ))

    if (!is.null(class)) {
        as(collapse, class)
    } else {
        collapse
    }
}



# Methods ====
#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("character"),
    .collapseToString)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("data.frame"),
    .collapseRows)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("DataFrame"),
    .collapseRows)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("grouped_df"),
    .collapseRows)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("integer"),
    .collapseToString)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("logical"),
    .collapseToString)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("matrix"),
    .collapseRows)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("numeric"),
    .collapseToString)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("tbl_df"),
    .collapseRows)
