# FIXME This still isn't working automatically for all common cases.
# See `plotGenderMarkers()` for example.



#' Match Arguments to Do Call
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @include standardizeCall.R
#' @export
#'
#'
#' @inheritParams standardizeCall
#' @inheritParams base::do.call
#' @inheritParams general
#' @param removeFormals `character`. Names of formal arguments to remove from
#'   `args` list before passing to `do.call()`.
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
#'
#' example <- function(object, xxx, ...) {
#'     do.call(
#'         what = paste,
#'         args = matchArgsToDoCall(
#'             args = list(collapse = " "),
#'             removeFormals = "xxx"
#'         )
#'     )
#' }
#' example(c("hello", "world"))
matchArgsToDoCall <- function(
    args,
    removeFormals = NULL,
    which = sys.parent(n = 1L),
    verbose = FALSE
) {
    assert_is_list(args)
    assert_is_non_empty(args)
    assert_has_names(args)
    assert_is_any_of(removeFormals, c("character", "NULL"))
    assert_is_a_number(which)
    assert_is_a_bool(verbose)

    list <- standardizeCall(
        which = which,
        return = "list",
        verbose = verbose
    )
    assert_is_list(list)
    definition <- list[["definition"]]
    assert_is_function(definition)
    call <- list[["match.call"]]
    assert_is_call(call)

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
            definition = definition,
            call = call,
            args = lapply(args, class)
        ))
    }

    assert_all_are_non_missing_nor_empty_character(names(args))
    assert_has_no_duplicates(names(args))

    args
}
