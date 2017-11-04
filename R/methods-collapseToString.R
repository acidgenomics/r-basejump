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
#' @param ... Additional arguments, passed to [glue::collapse()].
#'
#' @return
#' - For vector: String.
#' - For column data: Single row.
#'
#' @seealso
#' - [base::toString()].
#' - [glue::collapse()].
#'
#' @examples
#' # character
#' groceries <- c("milk", "eggs", "eggs", "veggies", NA)
#' collapseToString(groceries)
#' collapseToString(groceries, unique = FALSE, sort = FALSE)
#'
#' # numeric
#' collapseToString(seq(1:5))
#' collapseToString(c(3.141593, 6.0221409e+23))
#'
#' # logical
#' collapseToString(c(TRUE, FALSE))
#' collapseToString(c(NA, NA))
#'
#' # NULL
#' collapseToString(NULL)
#'
#' # data.frame
#' # Objects supporting `dim` function similarly
#' mtcars %>%
#'     head() %>%
#'     collapseToString() %>%
#'     t()
NULL



# Constructors ====
#' @importFrom glue collapse
#' @importFrom stats na.omit
#' @importFrom stringr str_replace_na
.collapseToString <- function(
    object,
    sep = ", ",
    unique = TRUE,
    sort = TRUE,
    ...) {
    if (length(object) == 1L) {
        return(object)
    }
    if (isTRUE(unique)) {
        object <- unique(object)
        if (length(object) == 1L) {
            return(object)
        }
    }
    if (isTRUE(sort)) {
        object <- sort(object)
    }
    collapse(object, sep, ...)
}



#' @importFrom dplyr funs mutate_all summarize_all
.collapseRows <- function(
    object,
    sep = ", ",
    unique = TRUE,
    sort = TRUE,
    ...) {
    origClass <- class(object)[[1L]]
    # Coerce to data.frame and perform manipulations
    object %>%
        as.data.frame() %>%
        mutate_all(funs(fixNA)) %>%
        summarize_all(funs(
            .collapseToString(
                object = .,
                sep = sep,
                unique = unique,
                sort = sort,
                ...))) %>%
        # Return as original class
        as(origClass)
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



setMethod(
    "collapseToString",
    signature("NULL"),
    function(object) {
        object
    })



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
