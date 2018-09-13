# FIXME Need to rethink this for S4...
# Can we get the generic and the method from the call stack?



#' Functions to Access the Function Call Stack
#'
#' @note
#' [base::match.call()] doesn't always work correctly inside S4 methods. Here we
#' are using a combination of [base::sys.call()] with [pryr::standardise_call()]
#' to correctly capture named arguments inside an S4 method.
#'
#' @author Michael Steinbaugh
#' @family Developer Functions
#' @export
#'
#' @inheritParams base::sys.call
#' @inheritParams general
#'
#' @seealso
#' - [base::sys.calls()].
#' - [base::sys.call()].
#' - [base::match.call()].
#'
#' @return `call`.
#'
#' @examples
#' x <- "XXX"
#' y <- "YYY"
#'
#' # Standard call
#' testing <- function(object, ...) {
#'     matchCallS4()
#' }
#' testing(x, y)
#'
#' # S4 mode
#' setGeneric(
#'     name = "testing",
#'     def = function(object, ...) {
#'         standardGeneric("testing")
#'     }
#' )
#'
#' setMethod(
#'     f = "testing",
#'     signature = signature("character"),
#'     definition = function(object, ...) {
#'         matchCallS4()
#'     }
#' )
#' testing(x, y)
#'
#' setMethod(
#'     f = "testing",
#'     signature = signature("character"),
#'     definition = function(object, xxx, ...) {
#'         matchCallS4()
#'     }
#' )
#' testing(x, y)
matchCallS4 <- function(
    f,
    signature,
    verbose = FALSE
) {

    stop()
    print(hasMethod())
    which <- sys.parent()

    # Print the call stack, for debugging.
    if (isTRUE(verbose)) {
        print(sys.status())
    }

    # Get the call.
    call <- sys.call(which = which)

    # Check for S4 `.local()` and recurse up an extra level, if necessary.
    if (identical(call[[1L]], as.symbol(".local"))) {
        if (isTRUE(verbose)) {
            message(".local detected")
        }
        which <- which - 1L
        call <- sys.call(which = which)
    }

    # Match (standardize) the call.
    # We need to be able to get the `.local` method expanded here.
    f <- eval(expr = call[[1L]], envir = sys.frame(which = which))
    if (is.primitive(f)) {
        return(call)
    }
    call <- match.call(definition = f, call = call)

    # Print the matched call, for debugging.
    if (isTRUE(verbose)) {
        print(which)
        print(call)
    }

    # Check call integrity before returning.
    assert_is_call(call)

    # Require that all arguments are named before returning.
    assert_all_are_non_missing_nor_empty_character(names(as.list(call)[-1L]))

    call
}
