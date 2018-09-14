.isLocalCall <- function(call) {
    stopifnot(is(call, "call"))
    identical(call[[1L]], as.symbol(".local"))
}



.standardizeCall <- function(
    definition = sys.function(sys.parent()),
    call = sys.call(sys.parent()),
    expand.dots = TRUE,
    envir = parent.frame(n = 2L),
    verbose = FALSE
) {
    # Print the call stack, for debugging.
    if (isTRUE(verbose)) {
        print(sys.status())
    }

    # Check for S4 `.local()` and move up the stack an extra level.
    if (.isLocalCall(call)) {
        which <- sys.parent(n = 2L)
        definition <- sys.function(sys.parent(n = which))
        # Pull the ".local" function out, which has the formals we need to
        # match against in `match.call()` below.
        definition <- .extractLocal(definition)
        call <- sys.call(which = which)
        envir <- sys.frame(which = which)
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

    # Print the matched call, for debugging.
    if (isTRUE(verbose)) {
        print(list(
            which = which,
            definition = definition,
            call = call,
            envir = envir
        ))
    }

    # Check call integrity before returning.
    assert_is_call(call)

    # Require that all arguments are named before returning.
    # This check is especially important for S4 methods containing `.local()`.
    assert_all_are_non_missing_nor_empty_character(names(as.list(call)[-1L]))

    call
}
