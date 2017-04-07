#' @rdname setNames
#'
#' @import tibble
#'
#' @param column Column to assign rownames
#'
#' @export
setRownames <- function(data, column) {
    # Setting rownames on a tibble is deprecated
    if (tibble::is_tibble(data)) {
        data <- as.data.frame(data)
    }
    rownames(data) <- data[[column]]
    return(data)
}
