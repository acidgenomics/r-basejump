#' Sanitize names
#'
#' This function prevents illegal characters from being set in variable names
#' and column or row names. Additionally, it will prepend an \code{X} character
#' if necessary
#'
#' @author Michael Steinbaugh
#'
#' @keywords internal
#'
#' @param data Data frame
#'
#' @return Sanitized data frame
#' @export
sanitizeNames <- function(data) {
    names(data) <- makeNamesSnake(names(data))
    if (is.matrix | is.data.frame) {
        rownames(data) <- makeNamesSnake(rownames(data))
    }
    return(data)
}
