#' Check if an object is a string
#'
#' @author Michael Steinbaugh
#'
#' @keywords internal
#'
#' @param object Generic object
#'
#' @return \code{TRUE/FALSE}
#' @export
#'
#' @examples
#' # string
#' isString("hello world")
#'
#' # numeric
#' isString(1)
#'
#' # character vector
#' isString(c("hello", "world"))
#'
#' # data.frame
#' isString(data.frame(1:2, 3:4))
#'
#' # list
#' isString(list(1, 2))
#'
#' # function
#' isString(base::sum)
isString <- function(object) {
    is.character(object) & length(object) == 1
}
