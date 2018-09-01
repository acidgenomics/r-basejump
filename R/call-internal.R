# Note that `match.call()` doesn't always work correctly inside S4 methods.
# Using an internal constructor here instead.
.matchCall <- function(which = -1L) {
    call <- sys.call(which = which)
    call <- standardise_call(call)
    assert_is_call(call)
    call
}



# Get a list of named arguments automatically.
# This is to be used inside S4 methods.
.matchArgs <- function(call = NULL, which = -1L) {
    if (is.null(call)) {
        # Need to recurse back 2 extra calls here.
        call <- .matchCall(which = which - 2L)
    }
    assert_is_call(call)
    args <- as.list(call)[-1L]
    assert_has_names(args)
    args
}
