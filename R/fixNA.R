#' Fix empty and NA character strings.
#'
#' @param string String missing \code{NA}.
#'
#' @return String containing \code{NA}.
#' @export
#'
#' @examples
#' fixNA(c(1, "x", "", "NA"))
fixNA <- function(string) {
    gsub("^$|^NA$", NA, string)
}



#' @rdname fixNA
#' @usage NULL
#' @export
fix_na <- fixNA
