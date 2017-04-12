#' Utility functions for toString
#'
#' @rdname toString
#'
#' @author Michael Steinbaugh
#'
#' @param character Character vector



#' @rdname toString
#' @return Unique character vector
#' @export
#' @examples
#' toStringUnique(c("milk", "eggs", "eggs", NA))
toStringUnique <- function(character) {
    character %>%
        unique %>%
        toString %>%
        gsub("NA,\\s|,\\sNA", "", .)
}



#' @rdname toString
#' @return Sorted unique character vector
#' @export
#' @examples
#' toStringSortUnique(c("milk", "eggs", "eggs", NA))
toStringSortUnique <- function(character) {
    character %>%
        unique %>%
        sort %>%
        toString %>%
        gsub("NA,\\s|,\\sNA", "", .)
}



#' @rdname toString
#' @param data Data with rows and columns (e.g. data frame, matrix)
#' @return Summarized data frame that has been collapsed to a single
#'   \code{toString()}-formatted row, separated by commas
#' @export
#' @examples
#' toStringSummarize(head(iris))
toStringSummarize <- function(data) {
    data %>%
        as.data.frame %>%
        summarise_each(funs(toStringUnique)) %>%
        mutate_each(funs(fixNA))
}
