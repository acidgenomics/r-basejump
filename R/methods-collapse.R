#' Collapse Utilities
#'
#' @rdname collapse
#'
#' @param sep Separator. Defaults to comma, like [base::toString()].
#' @param unique Unique values.
#' @param sort Sort values.
#' @param keepNA Keep `NA` values.
#'
#' @return
#' - Vector: String.
#' - Column data: Single row.
#'
#' @seealso
#' - [base::toString()].
#' - [glue::collapse()].
#'
#' @examples
#' groceries <- c("milk", "eggs", "eggs", "veggies", NA)
#'
#' collapse(groceries)
#' collapse(groceries, sort = )
#'
#' mtcars %>% head %>% collapse %>% t



#' @rdname collapse
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
            if (length(x) == 1) {
                return(x)
            }
        }
    } else if (length(x) == 1L) {
        return(x)
    } else {
        return(NA)
    }

    # Sort, if desired
    if (isTRUE(sort)) {
        x <- sort(x)
    }

    glue::collapse(x, sep, ...)
}



#' @rdname collapse
.collapseCols <- function(object, ...) {
    tbl <- as(object, "tibble")
    tbl[["rowname"]] <- NULL
    tbl %>%
        mutate_all(funs(fixNA)) %>%
        summarise_all(funs(.collapseVec(., ...)))
}



#' @rdname collapse
#' @export
setMethod("collapse", "character", .collapseVec)

#' @rdname collapse
#' @export
setMethod("collapse", "integer", .collapseVec)

#' @rdname collapse
#' @export
setMethod("collapse", "data.frame", function(object) {
    object %>% .collapseCols %>% as.data.frame
})

#' @rdname collapse
#' @export
setMethod("collapse", "DataFrame", function(object) {
    object %>% .collapseCols %>% as("DataFrame")
})

#' @rdname collapse
#' @export
setMethod("collapse", "matrix", function(object) {
    object %>% .collapseCols %>% as.matrix
})

#' @rdname collapse
#' @export
setMethod("collapse", "tbl_df", .collapseCols)
