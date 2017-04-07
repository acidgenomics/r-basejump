#' @rdname setNames
#'
#' @param data Data type that supports name assignments
#'
#' @return data Unmodified data with reformatted names
#' @export
#'
#' @examples
#' setNamesCamel(head(iris))
setNamesCamel <- function(data) {
    setNames(data, makeNamesCamel(colnames(data)))
}
