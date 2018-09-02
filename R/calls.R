#' Functions to Access the Function Call Stack
#'
#' Note that [base::match.call()] doesn't always work correctly inside S4
#' methods. Here we are using a combination of [base::sys.call()] with
#' [pryr::standardise_call()] to correctly capture named arguments inside an S4
#' method.
#'
#' @name calls
#' @author Michael Steinbaugh
#' @family Developer Functions
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
#'
#' @examples
#' # matchCall ====
#' f <- function(object) {
#'     matchCall(which = -1L)
#' }
#' f(object = "XXX")
#'
#' # matchArgs ====
#' f <- function(object) {
#'     matchArgs(which = -1L)
#' }
#' f(object = "XXX")
NULL



#' @rdname calls
#' @export
matchCall <- function(which = -1L) {
    assert_is_a_number(which)
    call <- sys.call(which = which)
    call <- standardise_call(
        call = call,
        env = pos.to.env(-1L)
    )
    assert_is_call(call)
    names <- names(as.list(call)[-1L])
    assert_all_are_non_empty_character(names)
    call
}



#' @rdname calls
#' @export
matchArgs <- function(which = -1L, S4 = FALSE) {
    assert_is_a_number(which)
    assert_is_a_bool(S4)
    # Need to recurse up an extra level here to capture the desired call.
    call <- matchCall(which = which - 1L)
    # If used inside an S4 method, check up the call stack.
    if (isTRUE(S4)) {
        # Check for `.local() call.
        assert_are_identical(
            x = sys.call(which = which - 1L)[[1L]],
            y = as.name(".local")
        )
        # Check for S4 generic.
        stopifnot(isS4(get(as.character(sys.call(which = -2L)[[1L]]))))
    }
    as.list(call)[-1L]
}
