#' toString call that outputs uniques.
#'
#' @import magrittr
#' @import stats
#'
#' @param x vector.
#'
#' @return string vector.
#' @export
toStringUnique <- function(x) {
    x %>%
        unique %>%
        toString(.) %>%
        gsub("NA,\\s|,\\sNA", "", .)
}
