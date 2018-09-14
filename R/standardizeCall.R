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
#' a <- "AAA"
#' b <- "BBB"
#' c <- "CCC"
#'
#' # Standard call
#' testing <- function(x, y) {
#'     print(match.call())
#'     print(standardizeCall())
#' }
#' testing(a, b)
#'
#' # S4 mode
#' setGeneric(
#'     name = "testing",
#'     def = function(x, y, ...) {
#'         standardGeneric("testing")
#'     }
#' )
#'
#' setMethod(
#'     f = "testing",
#'     signature = signature("character"),
#'     definition = function(x, y, ...) {
#'         print(match.call())
#'         print(standardizeCall())
#'     }
#' )
#' testing(a, b)
#'
#' setMethod(
#'     f = "testing",
#'     signature = signature("character"),
#'     definition = function(x, y, z) {
#'         print(match.call())
#'         print(match.call(call = sys.call(sys.parent())))
#'         print(standardizeCall())
#'     }
#' )
#' testing(a, b, c)
standardizeCall <- function(verbose = FALSE) {
    # Print the call stack, for debugging.
    if (isTRUE(verbose)) {
        print(list(
            sys.status = sys.status(),
            sys.parent = sys.parent(),
            definition = definition,
            call = call,
            envir = envir,
            isLocalCall = .isLocalCall(call)
        ))
    }

    # Check for S4 `.local()` and move up the stack an extra level accordingly.
    if (.isLocalCall(call)) {
        # FIXME This approach doesn't work recursively...
        which <- sys.parent(n = 2L)
        definition <- sys.function(sys.parent(n = which))
        stopifnot(is(definition, "MethodDefinition"))
        # Pull the ".local" function out, which has the formals we need to
        # match against in `match.call()` below.
        definition <- .extractLocal(definition)
        call <- sys.call(which = which)
        if (isTRUE(.isLocalCall(call))) {
            which <- which - 1L
            call <- sys.call(which = which)
        }
        envir <- sys.frame(which = which)
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
