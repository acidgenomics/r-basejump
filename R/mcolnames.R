#' Metadata column names
#'
#' @inheritParams params
#'
#' @seealso
#' - [S4Vectors::mcols()].
#'
#' @examples
#' data(rse)
#' mcolnames(rse)
#'
#' ## Asignment method.
#' mcolnames(rse) <- snake(mcolnames(rse))
#' mcolnames(rse)
mcolnames <- function(object) {
    # Check that object class supports `mcols()`.
    try <- try(expr = mcols(object), silent = TRUE)
    if (is(try, "try-error")) {
        stop(paste(class(object)[[1L]], "does not support mcols()."))
    }
    colnames(mcols(object))
}



#' @rdname mcolnames
#' @export
`mcolnames<-` <- function(object, value) {
    assert(isCharacter(value))
    # Check that object class supports `mcols()`.
    invisible(mcolnames(object))
    colnames(mcols(object)) <- value
    object
}
