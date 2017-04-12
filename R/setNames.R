#' setNames utility functions
#'
#' @rdname setNames
#'
#' @author Michael Steinbaugh
#'
#' @param data Data type that supports name assignments
#'
#' @return Unmodified data with reformatted names



#' @rdname setNames
#' @export
#' @examples
#' setNamesCamel(head(iris))
setNamesCamel <- function(data) {
    setNames(data, makeNamesCamel(colnames(data)))
}



#' @rdname setNames
#' @export
#' @examples
#' setNamesDot(head(iris))
setNamesDot <- function(data) {
    setNames(data, makeNames(colnames(data)))
}



#' @rdname setNames
#' @export
#' @examples
#' setNamesSnake(head(iris))
setNamesSnake <- function(data) {
    setNames(data, makeNamesSnake(colnames(data)))
}



#' @rdname setNames
#' @keywords internal
#' @usage NULL
#' @export
setColnames <- magrittr::set_colnames



#' @rdname setNames
#' @keywords internal
#' @usage NULL
#' @export
setRownames <- magrittr::set_rownames
