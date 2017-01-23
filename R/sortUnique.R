#' Sort unique
#' @export
#' @importFrom stats na.omit
#' @param x vector with duplicates, `NA` values
#' @return vector
sortUnique <- function(x) {
    x %>%
        stats::na.omit(.) %>%
        sort(.) %>%
        unique(.)
}
