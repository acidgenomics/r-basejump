#' Standardize Call
#'
#' This function adds matching support for S4 methods with formals that aren't
#' identical to the generic, and use a nested `.local()` call.
#'
#' @export
#'
#' @inheritParams base::sys.call
#' @inheritParams general
#'
#' @return
#' - `call`: `call`. Matched call.
#' - `list`: `list`. Verbose list that includes additional information about how
#'   the call was standardized.
#'
#' @seealso
#' - [base::match.call].
#' - [base::sys.call].
#' - [base::sys.parent].
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
standardizeCall <- function(
    which = sys.parent(n = 1L),
    return = c("call", "list"),
    verbose = FALSE
) {
    assert_is_a_number(which)
    assert_all_are_non_negative(which)
    if (which < 1L) {
        which <- 1L
    }
    return <- match.arg(return)
    assert_is_a_bool(verbose)

    # Determine where the call is in the stack that we want to standardize.
    # Note that this differs for S4 methods containing a nested `.local()`.
    .local <- .isLocalCall(sys.call(which = which))
    if (isTRUE(.local) && which > 1L) {
        which <- which - 1L
    }

    # Local the parameters we need to sanitize call.
    definition <- sys.function(which = which)
    call <- sys.call(which = which)
    envir <- sys.frame(which = which)

    list <- list(
        sys.status = sys.status(),
        sys.nframe = sys.nframe(),
        sys.parent = sys.parent(),
        .local = .local,
        which = which,
        definition = definition,
        call = call,
        envir = envir
    )

    # Extract the definition from `.local()`, if necessary.
    if (isTRUE(.local)) {
        stopifnot(!isTRUE(.isLocalCall(call)))
        # Update definition.
        if (is(definition, "MethodDefinition")) {
            # Pull the ".local()" function out, which has the formals we need to
            # match against in `match.call()` below.
            definition <- .extractLocal(definition)
            list[["definition"]] <- definition
        }
    }

    if (isTRUE(verbose)) {
        print(list)
    }

    # Now ready to match (expand) the call.
    # @seealso `pryr::standardise_call()`.
    # Note that we need to use the `envir` argument to properly match.
    call <- match.call(
        definition = definition,
        call = call,
        expand.dots = TRUE,
        envir = envir
    )
    list[["match.call"]] <- call

    if (isTRUE(verbose)) {
        print(list(match.call = call))
    }

    # Check call integrity before returning.
    assert_is_call(call)

    # Require that all arguments are named before returning.
    # This check is especially important for S4 methods containing `.local()`.
    assert_all_are_non_missing_nor_empty_character(names(as.list(call)[-1L]))

    if (return == "list") {
        list
    } else {
        call
    }
}



.isLocalCall <- function(call) {
    stopifnot(is(call, "call"))
    identical(call[[1L]], as.symbol(".local"))
}
