#' toString call that outputs uniques.
#'
#' @import magrittr
#' @importFrom stats na.omit
#'
#' @param x vector.
#'
#' @return string vector.
#' @export
toStringUnique <- function(x) {
    x %>%
        unique(.) %>%
        toString(.) %>%
        gsub("NA,\\s|,\\sNA", "", .)
}
