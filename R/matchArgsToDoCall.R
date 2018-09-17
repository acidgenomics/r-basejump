#' Match Arguments to Do Call
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @include standardizeCall.R
#' @export
#'
#'
#' @inheritParams base::sys.call
#' @inheritParams base::do.call
#' @inheritParams standardizeCall
#' @inheritParams general
#' @param removeFormals `character`. Names of formal arguments to remove from
#'   `args` list before passing to `do.call()`.
#'
#'
#' @return `list`. Arguments to pass to [do.call()].
#'
#' @seealso
#' - [base::do.call()].
#' - [base::sys.call()].
#' - [base::sys.parent()].
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
    n = 1L,
    definition = NULL,
    call = NULL,
    verbose = FALSE
) {
    assert_is_list(args)
    assert_is_non_empty(args)
    assert_has_names(args)
    assert_is_any_of(removeFormals, c("character", "NULL"))
    assert_is_a_number(n)
    assert_all_are_positive(n)
    assert_is_any_of(definition, c("function", "NULL"))
    assert_is_any_of(call, c("call", "NULL"))
    assert_is_a_bool(verbose)

    if (
        is.null(definition) &&
        is.null(call)
    ) {
        # Handle S4 `.local()`, if necessary.
        if (
            n == 1L &&
            isTRUE(.isLocalCall(sys.call(sys.parent(n = n))))
        ) {
            n <- n + 1L
        }

        # Get the position in the stack.
        which <- sys.parent(n = n)

        list <- standardizeCall(
            definition = sys.function(which = which),
            call = sys.call(which = which),
            expand.dots = TRUE,
            envir = sys.frame(which = which),
            return = "list",
            verbose = verbose
        )

        # Get the formals from the definition.
        definition <- list[["definition"]]
        assert_is_function(definition)

        # Get the matched (standardized) call.
        call <- list[["match.call"]]
        assert_is_call(call)
    } else {
        assert_is_function(definition)
        assert_is_call(call)
    }

    # Prepare the `args` list.
    callArgs <- as.list(call)[-1L] %>%
        .[setdiff(names(.), names(args))]
    args <- c(args, callArgs)
    formalArgs <- formals(definition) %>%
        # Currently removing "...". We may want to change this.
        .[setdiff(names(.), "...")] %>%
        .[setdiff(names(.), names(args))]
    args <- c(args, formalArgs)
    # Remove formals we want to exclude.
    args <- args[setdiff(names(args), removeFormals)]

    # Enable verbose mode, for debugging.
    if (isTRUE(verbose)) {
        print(list(
            call = call,
            args = lapply(args, class)
        ))
    }

    assert_all_are_non_missing_nor_empty_character(names(args))
    assert_has_no_duplicates(names(args))

    args
}
