#' Access to the Formal Arguments of an S4 Method
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams methods::getMethod
#'
#' @return `list` of formal arguments.
#' @export
#'
#' @seealso Modified version of [John Chambers' code](https://goo.gl/ymX571).
#'
#' @examples
#' x <- methodFormals("geometricMean", "numeric")
#' class(x)
#' names(x)
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
        warning("Expected `.local` assignment to be a function")
    }
    genFormals
}
