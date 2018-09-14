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
matchArgsToDoCall <- function(args, removeFormals = NULL) {
    assert_is_list(args)
    assert_is_non_empty(args)
    assert_has_names(args)
    assert_is_any_of(removeFormals, c("character", "NULL"))
    assert_is_a_bool(verbose)

    # Standardize the call.
    call <- standardizeCall(
        definition = definition,
        call = call,
        expand.dots = expand.dots,
        envir = envir,
        verbose = verbose
    )

    callArgs <- call %>%
        as.list() %>%
        .[-1L] %>%
        .[setdiff(names(.), names(args))]
    args <- c(args, callArgs)

    # We may need to rethink this step when dealing with `.local()`.
    formalArgs <- definition %>%
        formals() %>%
        .[setdiff(names(.), names(args))]
    args <- c(args, formalArgs)

    # Note that we're currently stripping "..." before passing to `do.call()`.
    args <- args[setdiff(
        x = names(args),
        y = c(removeFormals, "...")
    )]

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
f <- c(f1, f2)
formals(matchArgsToDoCall) <- f

