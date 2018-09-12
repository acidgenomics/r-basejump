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

    # Print the matched call, for debugging.
    if (isTRUE(verbose)) {
        print(call)
    }

    assert_is_call(call)
    call
}
