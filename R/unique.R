#' Vector uniques.
#'
#' @import magrittr
#' @importFrom stats na.omit
#'
#' @param x vector with duplicates, NA values.
#'
#' @return vector.
#' @export
unique <- function(x) {
    x %>%
        stats::na.omit(.) %>%
        base::unique(.)
}
