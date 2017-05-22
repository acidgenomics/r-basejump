#' Convert an R object to a character string
#'
#' Helper functions for [format()] to produce a single character string
#' describing an R object.
#'
#' @rdname toString
#' @name toString
#'
#' @param x The object to be converted.
#' @param ... Optional arguments passed to or from methods.
#'
#' @return A character vector of length 1 is returned.



#' @rdname toString
#' @export
#' @examples
#' toStringUnique(c("milk", "eggs", "eggs", NA))
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
#' @examples
#' toStringSortUnique(c("milk", "eggs", "eggs", NA))
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
#' Summarize columns with [toString()]
#'
#' Collapse column data into a single row summary columnwize using
#' [toString()].
#'
#' @rdname toStringSummarize
#'
#' @param x Data with rows and columns (e.g. data frame, matrix)
#' @param sort Sort the collapsed results.
#'
#' @return Summarized data frame collapsed to a single row.
#' @export
#'
#' @examples
#' mtcars %>% head %>% toStringSummarize
toStringSummarize <- function(x, sort = TRUE) {
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
