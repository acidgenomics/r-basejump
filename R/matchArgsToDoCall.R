# FIXME This still isn't working right recursively.
# S4 call stack inheritance is inception...



#' Match Arguments to Do Call
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @include standardizeCall.R
#' @export
#'
#' @inheritParams standardizeCall
#' @inheritParams general
#' @param removeFormals `character`. Names of formal arguments to remove from
#'   `args` list before passing to `do.call()`.
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
#'         removeFormals = "xxx"
#'     )
#'     print(args)
#'     do.call(what = paste, args = args)
#' }
#' example(c("hello", "world"))
matchArgsToDoCall <- function(
    args,
    removeFormals = NULL,
    call
) {
    assert_is_list(args)
    assert_is_non_empty(args)
    assert_has_names(args)
    assert_is_any_of(removeFormals, c("character", "NULL"))
    assert_is_a_bool(verbose)

    if (missing(call)) {
        # Standardize the call.
        call <- standardizeCall(
            definition = definition,
            expand.dots = expand.dots,
            envir = envir,
            verbose = verbose
        )
    }

    # Prepare the `args` list.
    callArgs <- as.list(call)[-1L] %>%
        .[setdiff(names(.), names(args))]
    args <- c(args, callArgs)
    # Remove formals we want to exclude.
    args <- args[setdiff(names(args), removeFormals)]
    # FIXME We may not be handling all passthrough formals here correctly...
    # Need to set up a unit test.
    # If that's the case, we can return `definition` from `standardizeCall`
    # return in a list and use that here.

    # Enable verbose mode, for debugging.
    if (isTRUE(verbose)) {
        print(list(
            call = call,
            formals = formals(definition),
            args = lapply(args, class)
        ))
    }

    assert_all_are_non_missing_nor_empty_character(names(args))
    assert_has_no_duplicates(names(args))

    args
}

# Assign the formals.
f1 <- formals(matchArgsToDoCall)
f2 <- formals(standardizeCall)
f2 <- f2[setdiff(names(f2), names(f1))]
f <- c(f1, f2)
formals(matchArgsToDoCall) <- f

