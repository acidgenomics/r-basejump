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
#' @inheritParams general
#'
#' @seealso
#' - [base::sys.calls()].
#' - [base::sys.call()].
#' - [base::match.call()].
#' - [pryr::standardise_call()].
#'
#' @return `call`.
#'
#' @examples
#' setGeneric(
#'     name = "testing",
#'     def = function(object, ...) {
#'         standardGeneric("testing")
#'     }
#' )
#' setMethod(
#'     f = "testing",
#'     signature = signature("character"),
#'     definition = function(object, ...) {
#'         call <- matchS4Call()
#'         call
#'     }
#' )
#' setMethod(
#'     f = "testing",
#'     signature = signature("numeric"),
#'     definition = function(object, xxx, ...) {
#'         call <- matchS4Call()
#'         call
#'     }
#' )
#' testing("abc")
#' testing(123L)
matchS4Call <- function(verbose = FALSE) {
    # Match against 1 level up in stack from system parent.
    which <- sys.parent() - 1L

    if (isTRUE(verbose)) {
        stack <- list()
        stack[["sys.calls"]] <- sys.calls()
        stack[["sys.function"]] <- sys.function(which = -1L)
        stack[["sys.parent"]] <- sys.parent()
        stack[["which"]] <- which
        print(stack)
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

    if (isTRUE(verbose)) {
        print(call)
    }

    call
}
