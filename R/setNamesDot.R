#' @rdname setNames
#' @export
#' @examples
#' setNamesDot(head(iris))
setNamesDot <- function(data) {
    setNames(data, makeNames(colnames(data)))
}
