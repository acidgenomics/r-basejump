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
    setNames(data, makeNamesCamel(names(data)))
}



#' @rdname setNames
#' @export
#' @examples
#' setNamesDot(head(iris))
setNamesDot <- function(data) {
    setNames(data, makeNames(names(data)))
}



#' @rdname setNames
#' @export
#' @examples
#' setNamesSnake(head(iris))
setNamesSnake <- function(data) {
    setNames(data, makeNamesSnake(names(data)))
}



#' @rdname setNames
#' @export
#' @examples
#' sanitizeNames(head(iris))
sanitizeNames <- function(data) {
    if (is.null(names(data))) {
        stop("object doesn't contain names")
    }

    # (Column) names
    names(data) <- makeNamesSnake(names(data))

    # Rows, if they exist
    if (!is.null(rownames(data))) {
        # Ignore numbered rownames
        if (!identical(rownames(data), as.character(1:nrow(data)))) {
            rownames(data) <- makeNamesSnake(rownames(data))
        }

    }

    return(data)
}



#' @rdname setNames
#' @keywords internal
#' @usage NULL
#' @export
setColnames <- set_colnames



#' @rdname setNames
#' @keywords internal
#' @usage NULL
#' @export
setRownames <- set_rownames
