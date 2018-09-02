#' Access to the Formal Arguments of an S4 Method
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @param f Generic `function` or `string` referencing a generic.
#' @param signature The signature of classes to match to the `f` argument.
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
    body <- body(method)
    if (
        is(body, "{") &&
        is(body[[2L]], "<-") &&
        identical(body[[2L]][[2L]], as.name(".local"))
    ) {
        local <- eval(body[[2L]][[3L]])
        if (is.function(local)) {
            return(formals(local))
        }
        # nocov start
        warning(paste(
            "Expected a .local assignment to be a function.",
            "Corrupted method?"
        ))
        # nocov end
    }
    genFormals  # nocov
}
