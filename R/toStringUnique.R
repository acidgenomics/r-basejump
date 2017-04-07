#' Utility functions for toString
#'
#' @rdname toString
#'
#' @author Michael Steinbaugh
#'
#' @param character Character vector
#'
#' @return Unique character vector
#' @export
#'
#' @examples
#' toStringUnique(c("milk", "eggs", "eggs", NA))
toStringUnique <- function(character) {
    character %>%
        unique %>%
        toString %>%
        gsub("NA,\\s|,\\sNA", "", .)
}
