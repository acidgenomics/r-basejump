#' Vector uniques.
#'
#' @import magrittr
#' @importFrom stats na.omit
#'
#' @param x vector with duplicates, NA values.
#'
#' @return vector.
#' @export
toUnique <- function(x) {
    x %>%
        stats::na.omit(.) %>%
        sort(.) %>%
        unique(.)
}
