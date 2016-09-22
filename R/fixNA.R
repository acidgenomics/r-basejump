#' Fix empty and "NA" character strings.
#'
#' @param a Values missing \code{NA}.
#'
#' @return Values containing \code{NA}.
#' @export
#'
#' @examples
#' fixNA(c(1, "x", "", "NA"))
fixNA <- function(a) {
    gsub("^$|^NA$", NA, a)
}
