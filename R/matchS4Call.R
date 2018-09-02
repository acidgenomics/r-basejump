#' Match S4 Call
#'
#' Note that [base::match.call()] doesn't always work correctly inside S4
#' methods. Here we are using a combination of [base::sys.call()] with
#' [pryr::standardise_call()] to correctly capture named arguments inside an S4
#' method.
#'
#' @name matchS4Call
#' @author Michael Steinbaugh
#' @family Developer Functions
#' @export
#'
#' @inheritParams base::sys.call
#'
#' @return `call`.
#' f <- function(x) {
#'     matchCall()
#' }
#' f(x = "XXX")
NULL



#' @rdname matchS4Call
#' @export
matchS4Call <- function(which = -2L) {
    # Check for `.local() call.
    assert_are_identical(
        x = sys.call(which = which + 1L)[[1L]],
        y = as.name(".local")
    )
    # Check for S4 generic.
    stopifnot(isS4(get(as.character(sys.call(which = which)[[1L]]))))
    call <- sys.call(which = which)
    call <- standardise_call(call = call)
    assert_is_call(call)
    call
}



# Match Arguments
# Get a list of named arguments automatically.
# This is to be used inside S4 methods.
#' @rdname matchS4Call
#' @export
matchS4Args <- function(which = -2L) {
    call <- matchS4Call(which = which - 1L)
    assert_is_call(call)
    args <- as.list(call)[-1L]
    assert_has_names(args)
    args
}
