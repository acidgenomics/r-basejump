#' Convert numeric to percentage
#'
#' @author Michael Steinbaugh
#'
#' @param number Number
#'
#' @return Percentage
#' @export
#'
#' @examples
#' percentage(0.1)
percentage <- function(number) {
    return(sprintf("%1.1f%%", number * 100))
}
