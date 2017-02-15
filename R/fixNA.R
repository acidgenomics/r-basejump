#' Fix empty and NA character strings
#' @export
#' @keywords general
#' @param string \code{string} missing \code{NA}.
#' @return \code{string} containing \code{NA}
#' @examples
#' fixNA(c(1, "x", "", "NA"))
fixNA <- function(string) {
    gsub("^$|^NA$", NA, string)
}
