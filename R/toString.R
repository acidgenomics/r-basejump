#' Convert an R object to a character string
#'
#' Helper functions for to produce a character string output for vectors and
#' data frames (via [toStringSummarize()]).
#'
#' @rdname toString
#'
#' @param x The object to be converted.
#' @param sep Separator used for collapse.
#' @param sort Optional arguments passed to or from methods.
#'
#' @return A character vector of length 1 is returned.
#' @export
#'
#' @examples
#' toStringUnique(c("milk", "eggs", "eggs", NA))
#' toStringSortUnique(c("milk", "eggs", "eggs", NA))
#'
#' mtcars %>% head %>% toStringSummarize
toStringUnique <- function(x, sep = ", ") {
    x %>%
        na.omit %>%
        unique %>%
        str_c(collapse = sep)
}

#' @rdname aliases
#' @usage NULL
#' @export
to_string_unique <- toStringUnique



#' @rdname toString
#' @export
toStringSortUnique <- function(x, sep = ", ") {
    x %>%
        na.omit %>%
        unique %>%
        sort %>%
        str_c(collapse = sep)
}

#' @rdname aliases
#' @usage NULL
#' @export
to_string_sort_unique <- toStringSortUnique






## Data frame manipulations ====
#' @rdname toString
#' @export
toStringSummarize <- function(x, sort = TRUE) {
    if (!any(is.data.frame(x) | is.matrix(x))) {
        stop("Column data required")
    }
    if (isTRUE(sort)) {
        fxn <- toStringSortUnique
    } else {
        fxn <- toStringUnique
    }
    x %>%
        summarise_all(funs(fxn)) %>%
        mutate_all(funs(fixNA))
}

#' @rdname aliases
#' @usage NULL
#' @export
to_string_summarize <- toStringSummarize

#' @rdname aliases
#' @usage NULL
#' @export
toStringSummarise <- toStringSummarize

#' @rdname aliases
#' @usage NULL
#' @export
to_string_summarise <- toStringSummarize
