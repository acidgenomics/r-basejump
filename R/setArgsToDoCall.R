# TODO Improve the assert checks to check the names passed in here.



#' Set Arguments to Do Call
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams BiocGenerics::do.call
#' @inheritParams general
#' @param removeArgs `character`. Names of arguments to remove from `call`
#'   and/or `fun` returns before passing to `do.call()`.
#' @param call `call`. Call to match against. This argument needs to be changed
#'   when called inside an S4 method.
#' @param fun `function`. Function containing the [do.call()] call. Recommended
#'   to leave unchanged by default.
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
#'         removeArgs = "xxx"
#'     )
#'     print(args)
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
        print(sys.status())
        print(list(
            names = names(args),
            call = call,
            fun = fun
        ))
    }

    assert_all_are_non_missing_nor_empty_character(names(args))
    assert_has_no_duplicates(names(args))
    args
}
