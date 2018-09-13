#' Access to the Data Inside an S4 Method Definition
#'
#' @name MethodDefinition
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams methods::selectMethod
#'
#' @return
#' `methodFunction()`: `function`. Function sealed inside `MethodDefinition`.
#' `methodFormals()`: `list`. Formal arguments.
#'
#' @seealso Modified version of [John Chambers' code](https://goo.gl/ymX571).
#'
#' @examples
#' # Function
#' x <- methodFunction(f = "geometricMean", signature = "numeric")
#' class(f)
#'
#' # Formals
#' x <- methodFormals(f = "geometricMean", signature = "numeric")
#' class(x)
#' names(x)
NULL



#' @rdname MethodDefinition
#' @export
methodFormals <- function(f, signature) {
    fun <- methodFunction(f, signature)
    formals(fun)
    fdef <- getGeneric(f)
    method <- selectMethod(fdef, signature)
    body <- body(method)
    if (
        is(body, "{") &&
        is(body[[2L]], "<-") &&
        identical(body[[2L]][[2L]], as.name(".local"))
    ) {
        # .local
        local <- eval(body[[2L]][[3L]])
        assert_is_function(local)
        formals(local)
    } else {
        formals(fdef)
    }
}



#' @rdname MethodDefinition
#' @export
methodFunction <- function(f, signature) {
    fdef <- getGeneric(f)
    method <- selectMethod(fdef, signature)
    body <- body(method)
    if (
        is(body, "{") &&
        is(body[[2L]], "<-") &&
        identical(body[[2L]][[2L]], as.name(".local"))
    ) {
        # .local
        fun <- eval(body[[2L]][[3L]])
    } else {
        fun <- slot(method, ".Data")
    }
    assert_is_function(fun)
    fun
}
