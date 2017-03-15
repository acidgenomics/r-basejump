#' Sort unique
#'
#' @author Michael Steinbaugh
#' @keywords general
#'
#' @importFrom stats na.omit
#'
#' @param x vector with duplicates, `NA` values
#'
#' @return vector
#' @export
sortUnique <- function(x) {
    x %>%
        stats::na.omit(.) %>%
        sort %>%
        unique
}
