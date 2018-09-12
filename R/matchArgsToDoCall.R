#' Match Arguments to Do Call
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams base::sys.call
#' @inheritParams BiocGenerics::do.call
#' @inheritParams general
#' @param removeArgs `character`. Names of arguments to remove from `call`
#'   and/or `fun` returns before passing to `do.call()`.
#'
#' @return `list`. Arguments to pass to [do.call()].
#'
#' @examples
#' example <- function(object, xxx, ...) {
#'     args <- matchArgsToDoCall(
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
matchArgsToDoCall <- function(
    args,
    removeArgs = NULL,
    which = sys.parent(),
    verbose = FALSE
) {
    assert_is_list(args)
    assert_is_non_empty(args)
    assert_has_names(args)
    assert_is_any_of(removeArgs, c("character", "NULL"))
    assert_is_a_number(which)
    assert_is_a_bool(verbose)

    call <- .sysCallWithS4(which = which, verbose = verbose)
    fun <- sys.function(which = which)

    callArgs <- call %>%
        as.list() %>%
        .[-1L] %>%
        .[setdiff(names(.), names(args))]
    args <- c(args, callArgs)

    formalArgs <- fun %>%
        formals() %>%
        .[setdiff(names(.), names(args))]
    args <- c(args, formalArgs)

    # Note that we're currently stripping "..." before passing to `do.call()`.
    args <- args[setdiff(
        x = names(args),
        y = c(removeArgs, "...")
    )]

    # Enable verbose mode, for debugging.
    if (isTRUE(verbose)) {
        print(list(
            call = call,
            fun = formals(fun),
            args = lapply(args, class)
        ))
    }

    assert_all_are_non_missing_nor_empty_character(names(args))
    assert_has_no_duplicates(names(args))
    args
}
