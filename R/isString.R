#' Check if an object is a string
#' @export
#' @param object Generic object
isString <- function(object) {
    is.character(object) & length(object) == 1
}
