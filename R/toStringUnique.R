## FIXME MOVE THIS TO ACIDBASE AND REEXPORT.



#' Convert to a unique character string
#'
#' @note Updated 2019-08-16.
#' @export
#'
#' @param object `atomic`.
#'
#' @seealso [`toString()`][base::toString].
#'
#' @return `character(1)`.
#'
#' @examples
#' toStringUnique(c("hello", "world", NA, "hello", "world", NA))
toStringUnique <- function(object) {
    assert(is.atomic(object))
    x <- object
    x <- as.character(x)
    x <- na.omit(x)
    x <- unique(x)
    x <- toString(x)
    x
}
