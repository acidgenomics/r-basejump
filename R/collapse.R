#' Collapse utilities
#'
#' Helper functions for to produce collapsed output for vectors ([toString()])
#' and data frames (summarizeRows()]).
#'
#' @rdname collapse
#'
#' @param x The object to be converted.
#' @param sep Separator used for collapse.
#'
#' @return A character vector of length 1 is returned.
#' @export
#'
#' @examples
#' toStringUnique(c("milk", "eggs", "eggs", NA))
#' toStringSortUnique(c("milk", "eggs", "eggs", NA))
#'
#' mtcars %>% head %>% summarizeRows
toStringUnique <- function(x, sep = ", ") {
    x %>%
        str_replace_na %>%
        unique %>%
        str_c(collapse = sep)
}

#' @rdname aliases
#' @usage NULL
#' @export
to_string_unique <- toStringUnique



#' @rdname collapse
#' @export
toStringSortUnique <- function(x, sep = ", ") {
    x %>%
        str_replace_na %>%
        unique %>%
        sort %>%
        str_c(collapse = sep)
}

#' @rdname aliases
#' @usage NULL
#' @export
to_string_sort_unique <- toStringSortUnique






## Data frame manipulations ====
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

#' @rdname aliases
#' @usage NULL
#' @export
summarize_rows <- summarizeRows

#' @rdname aliases
#' @usage NULL
#' @export
summariseRows <- summarizeRows

#' @rdname aliases
#' @usage NULL
#' @export
summarise_rows <- summarizeRows
