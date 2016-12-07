#' toString call that only outputs uniques.
#'
#' @import magrittr
#'
#' @param x vector.
#'
#' @return string vector.
#' @export
toStringUnique <- function(x) {
    x %>%
        unique %>%
        toString %>%
        gsub("NA,\\s|,\\sNA", "", .)
}
