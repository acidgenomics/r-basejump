#' Set names as dot notation
#'
#' @author Michael Steinbaugh
#'
#' @param data Data type that supports name assignments
#'
#' @return data Unmodified data with reformatted dot notation names
#' @export
setNamesDot <- function(data) {
    setNames(data, makeNames(colnames(data)))
}
