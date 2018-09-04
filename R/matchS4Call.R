#' Functions to Access the Function Call Stack
#'
#' Note that [base::match.call()] doesn't always work correctly inside S4
#' methods. Here we are using a combination of [base::sys.call()] with
#' [pryr::standardise_call()] to correctly capture named arguments inside an S4
#' method. The function raverses up the call stack and find the first
#' `.local()`. The desired S4 call to match is one level above.
#'
#' @author Michael Steinbaugh
#' @family Developer Functions
#' @export
#'
#' @inheritParams base::sys.call
#'
#' @seealso
#' - [base::sys.calls()].
#' - [base::sys.call()].
#' - [base::match.call()].
#' - [pryr::standardise_call()].
#'
#' @return
#' - [matchCall()]: `call`.
#' - [matchArgs()]: `list`.
matchS4Call <- function() {
    # Find the `.local()` function in the call stack.
    isLocal <- sapply(
        X = rev(sys.calls()),
        FUN = function(x) {
            identical(x[[1L]], as.name(".local"))
        }
    )
    which <- match(x = TRUE, table = isLocal)
    assert_is_a_number(which)
    # Flip back to negative index.
    which <- -which

    # Confirm the `.local() call.
    assert_are_identical(
        x = sys.call(which = which + 1L)[[1L]],
        y = as.name(".local")
    )

    # Ready to match the call.
    call <- sys.call(which = which)
    call <- standardise_call(
        call = call,
        env = pos.to.env(-1L)
    )
    assert_is_call(call)

    # Check that all arguments are named before returning.
    names <- names(as.list(call)[-1L])
    assert_all_are_non_empty_character(names)

    call
}
