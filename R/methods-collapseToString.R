#' Collapse to String
#'
#' @rdname collapseToString
#' @name collapseToString
#' @family Data Manipulation Utilities
#'
#' @inheritParams AllGenerics
#' @param sep Separator. Defaults to comma.
#' @param unique Unique values.
#' @param sort Sort values.
#' @param keepNA Keep `NA` values.
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
#' # Designed primarily for `dims` objects
#' mtcars %>%
#'     head() %>%
#'     collapseToString() %>%
#'     t()
#'
#' # Vectors are also supported
#' groceries <- c("milk", "eggs", "eggs", "veggies", NA)
#' collapseToString(groceries)
#' collapseToString(groceries, unique = FALSE, sort = FALSE)
NULL



# Constructors ====
.collapseVec <- function(
    object,
    sep = ", ",
    unique = TRUE,
    sort = TRUE,
    keepNA = FALSE,
    ...) {
    x <- as.character(object)

    # Handle NA values
    if (isTRUE(keepNA)) {
        x <- str_replace_na(x)
    } else {
        x <- na.omit(x)
    }

    # Extract uniques, if desired
    if (length(x) > 1L) {
        if (isTRUE(unique)) {
            x <- unique(x)
            if (length(x) == 1L) {
                return(x)
            }
        }
    } else if (length(x) == 1L) {
        return(x)
    }

    # Sort, if desired
    if (isTRUE(sort)) {
        x <- sort(x)
    }

    glue::collapse(x, sep, ...)
}



.collapseDim <- function(
    object,
    sep = ", ",
    unique = TRUE,
    sort = TRUE,
    keepNA = FALSE,
    ...) {
    origClass <- class(object)[[1L]]
    # Coerce to data.frame and perform manipulations
    object %>%
        as.data.frame() %>%
        mutate_all(funs(fixNA)) %>%
        summarize_all(funs(
            .collapseVec(
                object = .,
                sep = sep,
                unique = unique,
                sort = sort,
                keepNA = keepNA,
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
    .collapseVec)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("integer"),
    .collapseVec)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("data.frame"),
    .collapseDim)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("DataFrame"),
    .collapseDim)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("matrix"),
    .collapseDim)



#' @rdname collapseToString
#' @export
setMethod(
    "collapseToString",
    signature("tbl_df"),
    .collapseDim)
