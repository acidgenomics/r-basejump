# TODO See if we can set default `call` and `fun` arguments.
# call = match.call
# fun = sys.function(sys.parent())



#' Set Arguments to Do Call
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams BiocGenerics::do.call
#' @inheritParams general
#' @param removeArgs `character`. Names of objects to remove from `call` (e.g.
#'   [match.call()]) and `fun` (e.g. [sys.function()]) before passing to
#'   `do.call()`.
#' @param call `call`. Call to match against. Recommended to use either
#'   [match.call()] or [matchS4Call()] (for S4 method).
#' @param fun `function`. Function containing the [do.call()] step. Recommended
#'   to use [sys.function()] by default.
#'
#' @return `list`. Arguments to pass to [do.call()].
#'
#' @examples
#' example <- function(object, xxx, ...) {
#'     args <- setArgsToDoCall(
#'         args = list(
#'             object = object,
#'             collapse = " "
#'         ),
#'         removeArgs = "xxx",
#'         verbose = TRUE
#'     )
#'     do.call(what = paste, args = args)
#' }
#' example(c("hello", "world"))
setArgsToDoCall <- function(
    args,
    removeArgs = NULL,
    call = sys.call(which = sys.parent()),
    fun = sys.function(which = sys.parent()),
    verbose = FALSE
) {
    assert_is_list(args)
    assert_has_names(args)
    assert_is_any_of(removeArgs, c("character", "NULL"))
    call <- standardise_call(call)
    assert_is_call(call)
    assert_is_function(fun)
    assert_is_a_bool(verbose)
    # TODO Improve the assert checks to check the names passed in here.

    callArgs <- call %>%
        as.list() %>%
        .[-1L] %>%
        .[setdiff(names(.), names(args))]
    args <- c(args, callArgs)

    formalArgs <- fun %>%
        formals() %>%
        .[setdiff(names(.), names(args))]
    args <- c(args, formalArgs)

    args <- args[setdiff(
        x = names(args),
        y = c(removeArgs, "...")
    )]

    # Enable verbose mode, for debugging.
    if (isTRUE(verbose)) {
        stack <- list(
            names = names(args),
            call = call,
            fun = fun
        )
        print(stack)
    }

    stopifnot(!any(duplicated(names(args))))
    args
}
