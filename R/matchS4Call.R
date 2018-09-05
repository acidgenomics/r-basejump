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
#' @return `call`.
#'
#' @examples
#' # Execute this inside an S4 method.
#' \dontrun{matchS4Call()}
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



# FIXME Remove this before pull request
# For testing purposes =========================================================
# S4 dispatches differently if the method doesn't match the generic exactly.
setGeneric(
    name = "matchS4CallTest",
    def = function(object, ...) {
        standardGeneric("matchS4CallTest")
    }
)


setMethod(
    "matchS4CallTest",
    signature("character"),
    function(object, ...) {
        call <- matchS4Call(verbose = TRUE)
        call
    }
)

setMethod(
    "matchS4CallTest",
    signature("numeric"),
    function(object, xxx, ...) {
        call <- matchS4Call(verbose = TRUE)
        call
    }
)
