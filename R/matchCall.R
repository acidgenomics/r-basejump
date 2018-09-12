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
    .sysCallWithS4(which = sys.parent(), verbose = verbose)
}
