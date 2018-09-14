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



.hasLocal <- function(definition) {
    assert_is_function(definition)
    body <- body(definition)
    is(body, "{") &&
        is(body[[2L]], "<-") &&
        identical(body[[2L]][[2L]], as.name(".local"))
}



.extractLocal <- function(definition) {
    stopifnot(.hasLocal(definition))
    body <- body(definition)
    local <- eval(body[[2L]][[3L]])
    assert_is_function(local)
    local
}



#' @rdname MethodDefinition
#' @export
methodFormals <- function(f, signature) {
    definition <- methodFunction(f, signature)
    formals(definition)
}



#' @rdname MethodDefinition
#' @export
methodFunction <- function(f, signature) {
    method <- selectMethod(
        f = getGeneric(f = f, mustFind = TRUE),
        signature = signature
    )
    stopifnot(is(method, "MethodDefinition"))
    # S4 will nest `.local()` function inside the method when the formals aren't
    # identical to the generic. Otherwise it will be slotted in ".Data".
    if (isTRUE(.hasLocal(method))) {
        fun <- .extractLocal(method)
    } else {
        # FIXME Check to see whether we need to pull out
        # fun <- slot(method, ".Data")
        fun <- method
    }
    assert_is_function(fun)
    fun
}
