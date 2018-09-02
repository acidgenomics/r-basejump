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
#' @param S4 `boolean`. Are we matching the call from inside an S4 method?
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
matchCall <- function(S4 = FALSE, which) {
    assert_is_a_bool(S4)

    # If `which` isn't specified, set the parent automatically.
    if (missing(which)) {
        if (isTRUE(S4)) {
            which <- -2L
        } else {
            which <- -1L
        }
    }
    assert_is_a_number(which)

    # If used inside an S4 method, check the call stack first.
    if (isTRUE(S4)) {
        # Check for `.local() call.
        assert_are_identical(
            x = sys.call(which = which + 1L)[[1L]],
            y = as.name(".local")
        )
        # Check for S4 generic.
        stopifnot(isS4(get(as.character(sys.call(which = which)[[1L]]))))
    }

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



#' @rdname calls
#' @export
matchArgs <- function(S4 = FALSE, which) {
    assert_is_a_bool(S4)
    # Note that we need to recurse up an extra level here.
    if (missing(which)) {
        if (isTRUE(S4)) {
            which <- -3L
        } else {
            which <- -2L
        }
    }
    call <- matchCall(S4 = S4, which = which)
    # FIXME
    # print(call)
    # print(sys.calls())
    as.list(call)[-1L]
}
