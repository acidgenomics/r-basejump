#' Access to the Formal Arguments of an S4 Method
#'
#' @inheritParams methods::getMethod
#'
#' @note Modified version of [John Chambers' code](https://goo.gl/ymX571).
#'
#' @return `list` of formal arguments.
#' @export
#'
#' @examples
#' methodFormals("geometricMean", "numeric")
methodFormals <- function(f, signature) {
    fdef <- getGeneric(f)
    method <- selectMethod(fdef, signature)
    genFormals <- formals(fdef)
    b <- body(method)
    if (
        is(b, "{") &&
        is(b[[2L]], "<-") &&
        identical(b[[2L]][[2L]], as.name(".local"))
    ) {
        local <- eval(b[[2L]][[3L]])
        if (is.function(local)) {
            return(formals(local))
        }
        warn("Expected `.local` assignment to be a function")
    }
    genFormals
}
