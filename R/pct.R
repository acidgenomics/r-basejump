#' Convert numeric to percentage.
#'
#' @param number Number.
#'
#' @return Percentage.
#' @export
#'
#' @examples
#' pct(0.1)
pct <- function(number) {
    return(sprintf("%1.1f%%", number * 100))
}
