#' Fix empty and NA character strings
#'
#' @author Michael Steinbaugh
#'
#' @param string String missing \code{NA}
#'
#' @return String containing \code{NA}
#' @export
#'
#' @examples
#' fixNA(c(1, "x", "", "NA"))
fixNA <- function(string) {
    gsub("^$|^NA$", NA, string)
}
