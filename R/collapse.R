#' Collapse utilities
#'
#' Helper functions for to produce collapsed output for vectors
#' ([base::toString()]) and data frames ([summarizeRows()]).
#'
#' @rdname collapse
#'
#' @param x The object to be converted.
#' @param sep Separator used for collapse.
#'
#' @return A character vector of length 1 is returned.
#'
#' @examples
#' groceries <- c("milk", "eggs", "eggs", NA)
#'
#' toStringUnique(groceries)
#' toStringSortUnique(groceries)
#'
#' mtcars %>% head %>% summarizeRows



#' @rdname collapse
#' @export
toStringUnique <- function(x, sep = ", ") {
    x %>%
        str_replace_na %>%
        unique %>%
        str_c(collapse = sep)
}

#' @rdname collapse
#' @usage NULL
#' @export
toStringUnique -> to_string_unique  # nolint



#' @rdname collapse
#' @export
toStringSortUnique <- function(x, sep = ", ") {
    x %>%
        str_replace_na %>%
        unique %>%
        sort %>%
        str_c(collapse = sep)
}

#' @rdname collapse
#' @usage NULL
#' @export
toStringSortUnique -> to_string_sort_unique   # nolint



#' @rdname collapse
#' @export
summarizeRows <- function(x, sep = ", ") {
    if (!any(is.data.frame(x) | is.matrix(x))) {
        stop("Column data required")
    }
    x %>%
        as_tibble %>%
        summarise_all(funs(toStringSortUnique(., sep = sep))) %>%
        mutate_all(funs(fixNA))
}

#' @rdname collapse
#' @usage NULL
#' @export
summarizeRows -> summariseRows

#' @rdname collapse
#' @usage NULL
#' @export
summarizeRows -> summarize_rows   # nolint

#' @rdname collapse
#' @usage NULL
#' @export
summarizeRows -> summarise_rows  # nolint
