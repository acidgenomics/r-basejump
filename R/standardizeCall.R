#' Standardize Call
#'
#' This function adds matching support for S4 methods with formals that aren't
#' identical to the generic, and use a nested `.local()` call.
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams base::match.call
#' @inheritParams general
#'
#' @seealso [base::match.call()].
#'
#' @examples
#' aaa <- "AAA"
#' bbb <- "BBB"
#'
#' # Standard function
#' testing <- function(a, b) {
#'     standardizeCall()
#' }
#' testing(aaa, bbb)
#'
#' # Inside S4 method
#' setGeneric(
#'     name = "testing",
#'     def = function(a, b, ...) {
#'         standardGeneric("testing")
#'     }
#' )
#'
#' setMethod(
#'     f = "testing",
#'     signature = signature("character"),
#'     definition = function(a, b, ...) {
#'         standardizeCall()
#'     }
#' )
#' testing(aaa, bbb)
standardizeCall <- function(verbose = FALSE) {
    # Print the call stack, for debugging.
    if (isTRUE(verbose)) {
        print(list(
            sys.status = sys.status(),
            sys.nframe = sys.nframe(),
            sys.parent.1 = sys.parent(n = 1L),
            sys.parent.2 = sys.parent(n = 2L),
            sys.parent.3 = sys.parent(n = 3L),
            definition = definition,
            call = call,
            envir = envir,
            isLocalCall = .isLocalCall(call)
        ))
    }

    # Check for S4 `.local()` and move up the stack an extra level accordingly.
    if (.isLocalCall(call)) {
        which <- sys.parent(n = 2L)
        definition <- sys.function(which = which)
        stopifnot(is(definition, "MethodDefinition"))
        # Pull the ".local" function out, which has the formals we need to
        # match against in `match.call()` below.
        definition <- .extractLocal(definition)
        call <- sys.call(which = which)
        stopifnot(!isTRUE(.isLocalCall(call)))
        envir <- sys.frame(which = which)
    } else {
        which <- sys.parent(n = 1L)
    }

    # Print the matched call, for debugging.
    if (isTRUE(verbose)) {
        print(list(
            which = which,
            definition = definition,
            call = call,
            envir = envir
        ))
    }

    # Now ready to match (expand) the call.
    # @seealso `pryr::standardise_call()`.
    # Note that we need to use the `envir` argument to properly match.
    call <- match.call(
        definition = definition,
        call = call,
        expand.dots = expand.dots,
        envir = envir
    )

    if (isTRUE(verbose)) {
        print(call)
    }

    # Check call integrity before returning.
    assert_is_call(call)

    # Require that all arguments are named before returning.
    # This check is especially important for S4 methods containing `.local()`.
    assert_all_are_non_missing_nor_empty_character(names(as.list(call)[-1L]))

    call
}

# Assign the formals.
# Ensure the function matches `base::match.call()`.
f <- formals(match.call)
f <- c(f, formals(standardizeCall))
formals(standardizeCall) <- f



.isLocalCall <- function(call) {
    stopifnot(is(call, "call"))
    identical(call[[1L]], as.symbol(".local"))
}
