#' Sort unique
#'
#' @author Michael Steinbaugh
#'
#' @keywords internal
#'
#' @importFrom stats na.omit
#'
#' @param x vector with duplicates, \code{NA} values
#'
#' @return vector
#' @export
sortUnique <- function(x) {
    x %>%
        stats::na.omit(.) %>%
        sort %>%
        unique
}
