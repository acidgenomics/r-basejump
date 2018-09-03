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
matchCall <- function(which, S4 = FALSE) {
    assert_is_a_bool(S4)

    # S4 mode: Traverse up the call stack and find the first `.local()`.
    # The desired S4 call to match is one level above.
    if (isTRUE(S4)) {
        isLocal <- sapply(
            X = rev(sys.calls()),
            FUN = function(x) {
                identical(x[[1L]], as.name(".local"))
            }
        )
        which <- match(x = TRUE, table = isLocal)
        assert_is_a_number(which)
        which <- -which
        # Check for `.local() call.
        assert_are_identical(
            x = sys.call(which = which + 1L)[[1L]],
            y = as.name(".local")
        )
    }

    assert_is_a_number(which)

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
matchArgs <- function(which, S4 = FALSE) {
    call <- matchCall(
        which = which,
        S4 = S4
    )
    as.list(call)[-1L]
}
