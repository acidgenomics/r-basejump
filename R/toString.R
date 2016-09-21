#' toString call that only outputs uniques.
#'
#' @import magrittr
#'
#' @param x vector.
#'
#' @return string vector.
#' @export
toString <- function(x) {
    x %>%
        base::unique(.) %>%
        base::toString(.) %>%
        gsub("NA,\\s|,\\sNA", "", .)
}
