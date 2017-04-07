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
