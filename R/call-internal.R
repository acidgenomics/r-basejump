.sysCallWithS4 <- function(which, verbose = FALSE) {
    # Print the call stack, for debugging.
    if (isTRUE(verbose)) {
        print(sys.status())
        print(which)
    }

    # Get the call.
    call <- sys.call(which = which)

    # Check for S4 `.local()` and recurse up an extra level, if necessary.
    if (identical(call[[1L]], as.symbol(".local"))) {
        which <- which - 1L
        call <- sys.call(which = which)
    }

    # Standardize the call with pryr package.
    # This will expand all formals, like `match.call()`.
    call <- standardise_call(call = call, env = sys.frame(which = which))

    # Print the matched call, for debugging.
    if (isTRUE(verbose)) {
        print(call)
    }

    # Check call integrity before returning.
    assert_is_call(call)
    # Require that all arguments are named before returning.
    assert_all_are_non_missing_nor_empty_character(names(as.list(call)[-1L]))

    call
}
