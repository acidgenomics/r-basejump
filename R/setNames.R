#' Naming utility functions.
#'
#' @rdname setNames
#'
#' @param data Data type that supports name assignments.
#'
#' @usage NULL
#'
#' @return Unmodified data with reformatted names.
#' @export
setNames <- set_names



#' @rdname setNames
#' @export
#' @examples
#' setNamesCamel(head(starwars))
setNamesCamel <- function(data) {
    setNames(data, makeNamesCamel(names(data)))
}



#' @rdname setNames
#' @usage NULL
#' @export
set_names_camel <- setNamesCamel



#' @rdname setNames
#' @export
#' @examples
#' setNamesDot(head(starwars))
setNamesDot <- function(data) {
    setNames(data, makeNames(names(data)))
}



#' @rdname setNames
#' @usage NULL
#' @export
set_names_dot <- setNamesDot



#' @rdname setNames
#' @export
#' @examples
#' setNamesSnake(head(starwars))
setNamesSnake <- function(data) {
    setNames(data, makeNamesSnake(names(data)))
}



#' @rdname setNames
#' @usage NULL
#' @export
set_names_snake <- setNamesSnake



#' @rdname setNames
#' @export
#' @examples
#' sanitizeNames(head(starwars))
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
#' @usage NULL
#' @export
sanitiseNames <- sanitizeNames



#' @rdname setNames
#' @usage NULL
#' @export
sanitize_names <- sanitizeNames



#' @rdname setNames
#' @usage NULL
#' @export
sanitise_names <- sanitizeNames



#' @rdname setNames
#' @usage NULL
#' @export
setColnames <- set_colnames



#' @rdname setNames
#' @usage NULL
#' @export
setRownames <- set_rownames
