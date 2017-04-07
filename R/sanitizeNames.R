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
