# FIXME This isn't working because we need to get the formals from .local
# Think about a different approach using the one we have in `methodFormals()`.

# function (x, ...)
# {
#     .local <- function (x, file, format, ...)
#     {

.sysCallWithS4 <- function(
    which,
    verbose = FALSE
) {
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
        call <- sys.call(which = which - 1L)
        # Standardize the call.
        # We need to be able to get the `.local` method expanded here.
        f <- eval(expr = call[[1L]], envir = sys.frame(which = which - 2L))
        if (is.primitive(f)) {
            return(call)
        }
        print(formals(f))
        call <- match.call(definition = f, call = call)
        print(call)
        stop()
    } else {
        # Standardize the call.
        f <- eval(expr = call[[1L]], envir = sys.frame(which = which))
        if (is.primitive(f)) {
            return(call)
        }
        call <- match.call(definition = f, call = call)
    }

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
