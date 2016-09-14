#' toString call that outputs uniques.
#' @import magrittr
#' @param x vector.
#' @return vector.
#' @export
toStringUnique <- function(x) {
    x <- x %>%
        unique %>%
        sort %>%
        toString(.)
    return(x)
}
