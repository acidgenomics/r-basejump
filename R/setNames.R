#' Naming utility functions.
#'
#' @rdname setNames
#'
#' @param object Object for which a [names()] attribute will be meaningful.
#'
#' @return Object with unmodified data and reformatted names.
#' @export
#'
#' @seealso [stats::setNames()]



#' @rdname setNames
#' @export
#' @examples
#' setNamesCamel(head(starwars))
setNamesCamel <- function(object) {
    setNames(object, makeNamesCamel(names(object)))
}



#' @rdname setNames
#' @usage NULL
#' @export
set_names_camel <- setNamesCamel



#' @rdname setNames
#' @export
#' @examples
#' setNamesDot(head(starwars))
setNamesDot <- function(object) {
    setNames(object, makeNames(names(object)))
}



#' @rdname setNames
#' @usage NULL
#' @export
set_names_dot <- setNamesDot



#' @rdname setNames
#' @export
#' @examples
#' setNamesSnake(head(starwars))
setNamesSnake <- function(object) {
    setNames(object, makeNamesSnake(names(object)))
}



#' @rdname setNames
#' @usage NULL
#' @export
set_names_snake <- setNamesSnake



#' @rdname setNames
#' @export
#' @examples
#' sanitizeNames(head(starwars))
sanitizeNames <- function(object) {
    if (is.null(names(object))) {
        stop("Object doesn't contain names")
    }

    # (Column) names
    names(object) <- makeNamesSnake(names(object))

    # Rows, if they exist
    if (!is.null(rownames(object))) {
        # Ignore numbered rownames
        if (!identical(rownames(object), as.character(1:nrow(object)))) {
            rownames(object) <- makeNamesSnake(rownames(object))
        }

    }

    return(object)
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
