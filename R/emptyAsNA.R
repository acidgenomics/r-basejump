#' Convert empty values to NA.
#'
#' @import dplyr
#'
#' @param x Values missing \code{NA}.
#'
#' @return Values containing \code{NA}.
#' @export
emptyAsNA <- function(x) {
    dplyr::if_else(x != "", x, NA)
}
