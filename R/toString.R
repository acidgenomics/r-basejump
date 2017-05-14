#' Convert an R object to a character string.
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
to_string <- toString



#' @rdname toString
#' @export
#' @examples
#' toStringUnique(c("milk", "eggs", "eggs", NA))
toStringUnique <- function(x) {
    x %>%
        unique %>%
        toString %>%
        str_replace_all("NA,\\s|,\\sNA", "")
}


#' @rdname toString
#' @export
to_string_unique <- toStringUnique



#' @rdname toString
#' @export
#' @examples
#' toStringSortUnique(c("milk", "eggs", "eggs", NA))
toStringSortUnique <- function(x) {
    x %>%
        unique %>%
        sort %>%
        toString %>%
        str_replace_all("NA,\\s|,\\sNA", "")
}



#' @rdname toString
#' @export
to_string_sort_unique <- toStringSortUnique






# Data frame manipulations ====

#' Summarize columns using [toString()].
#'
#' Collapse column data into a single row summary columnwize using
#' [toString()].
#'
#' @rdname toStringSummarize
#'
#' @param data Data with rows and columns (e.g. data frame, matrix)
#'
#' @return Summarized data frame that has been collapsed to a single
#'   [toString()]-formatted row, separated by commas.
#' @export
#'
#' @examples
#' toStringSummarize(head(starwars))
toStringSummarize <- function(data) {
    data %>%
        as.data.frame %>%
        summarise_all(funs(toStringUnique)) %>%
        mutate_all(funs(fixNA))
}



#' @rdname toStringSummarize
#' @export
toStringSummarise <- toStringSummarize



#' @rdname toStringSummarize
#' @export
to_string_summarize <- toStringSummarize



#' @rdname toStringSummarize
#' @export
to_string_summarise <- toStringSummarize
