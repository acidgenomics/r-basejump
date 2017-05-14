#' Fix empty and `NA` character strings.
#'
#' @param string String missing `NA`.
#'
#' @return String containing `NA`.
#' @export
#'
#' @examples
#' fixNA(c(1, "x", "", "NA"))
fixNA <- function(string) {
    gsub("^$|^NA$", NA, string)
}



#' @rdname fixNA
#' @export
fix_na <- fixNA
