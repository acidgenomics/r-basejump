#' Functions to Access the Function Call Stack
#'
#' Note that [base::match.call()] doesn't always work correctly inside S4
#' methods. Here we are using a combination of [base::sys.call()] with
#' [pryr::standardise_call()] to correctly capture named arguments inside an S4
#' method.
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
#' x <- "XXX"
#'
#' # Standard call
#' testing <- function(object, ...) {
#'     matchCall()
#' }
#' testing(x)
#'
#' # S4 mode
#' setGeneric(
#'     name = "testing",
#'     def = function(object, ...) {
#'         standardGeneric("testing")
#'     }
#' )
#'
#' setMethod(
#'     f = "testing",
#'     signature = signature("character"),
#'     definition = function(object, ...) {
#'         matchCall()
#'     }
#' )
#' testing(x)
#'
#' setMethod(
#'     f = "testing",
#'     signature = signature("character"),
#'     definition = function(object, xxx, ...) {
#'         matchCall()
#'     }
#' )
#' testing(x)
matchCall <- function(verbose = FALSE) {
    which <- sys.parent()
    call <- .sysCallWithS4(which = which, verbose = verbose)
    # Standardize with pryr package.
    call <- standardise_call(call = call, env = sys.frame(which = which))
    # Print the matched call, for debugging.
    if (isTRUE(verbose)) {
        print(call)
    }
    # Require that all arguments are named before returning.
    assert_all_are_non_missing_nor_empty_character(names(as.list(call)[-1L]))
    call
}
