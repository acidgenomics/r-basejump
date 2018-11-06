# dots =========================================================================
#' Extract Dots from Function
#'
#' @export
#'
#' @param ... Objects as dots.
#' @param character `boolean`. Return dots (`...`) as `character`.
#'
#' @return
#' - "`character = FALSE`": `list`. Objects as `name` class. Can return the
#'   object from the `name` with [eval()].
#' - "`character = TRUE`": `character`. Names of the dots.
#'
#' @seealso
#' - `help("dotsMethods", "methods")`.
#' - [tidyverse](http://tidyverse.org) documentation:
#'   - [rlang](http://rlang.tidyverse.org).
#'   - [dplyr utils](https://goo.gl/fhAuak).
#'   - [devtools infrastructure](https://goo.gl/bM5TrP).
#'
#' @examples
#' dots(a, b, c, character = FALSE)
#' dots(a, b, c, character = TRUE)
dots <- function(..., character = FALSE) {
    dots <- eval_bare(substitute(alist(...)))
    assert_is_list(dots)
    assert_is_non_empty(dots)
    assert_has_no_duplicates(dots)
    invisible(lapply(dots, assert_is_name))

    # Convert names (symbols) to character.
    names <- vapply(dots, as.character, character(1L))
    assert_has_no_duplicates(names)

    if (isTRUE(character)) {
        names
    } else {
        dots
    }
}



# matchArgsToDoCall ============================================================
#' Match Arguments to Do Call
#' @export
#'
#' @inheritParams standardizeCall
#' @inheritParams base::do.call
#' @inheritParams params
#' @param removeFormals `character`. Names of formal arguments to remove from
#'   `args` list before passing to `do.call()`.
#'
#' @return `list`. Arguments to pass to [base::do.call()].
#'
#' @seealso
#' - [standardizeCall()].
#' - [base::do.call()].
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
    args = NULL,
    removeFormals = NULL,
    which = sys.parent(n = 1L),
    verbose = FALSE
) {
    assert_is_any_of(args, c("list", "NULL"))
    if (is.list(args)) {
        assert_is_non_empty(args)
        assert_has_names(args)
    } else {
        args <- list()
    }
    assert_is_any_of(removeFormals, c("character", "NULL"))
    assert_is_a_number(which)
    if (which < 1L) {
        which <- 1L
    }
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
        .[setdiff(names(.), "...")] %>%
        .[setdiff(names(.), names(args))]
    args <- c(args, formalArgs)
    # Remove formals we want to exclude.
    args <- args[setdiff(names(args), removeFormals)]

    # Show the unevaluated args, if desired.
    if (isTRUE(verbose)) {
        print(list(args = lapply(args, class)))
    }

    # Ensure all arguments are evaluated.
    # Missing or NULL arguments will be stripped.
    # https://stackoverflow.com/questions/16740307
    envir <- sys.frame(which = which)
    args <- lapply(
        X = args,
        FUN = function(expr) {
            if (
                is.call(expr) ||
                is.name(expr) ||
                is.symbol(expr)
            ) {
                # Evaluate, if necessary.
                tryCatch(
                    expr = eval(expr = expr, envir = envir),
                    error = function(e) NULL
                )
            } else {
                expr
            }
        }
    )
    # Remove any `NULL` arguments. We may want to consider changing this
    # approach in the future, in case passing `NULL` through is important.
    args <- Filter(f = Negate(is.null), x = args)

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
    invisible(lapply(
        X = args,
        FUN = function(x) {
            assert_that(!is.call(x))
            assert_that(!is.name(x))
            assert_that(!is.symbol(x))
        }
    ))

    args
}



# standardizeCall ==============================================================
#' Standardize Call
#'
#' This function adds matching support for S4 methods with formals that aren't
#' identical to the generic, and use a nested `.local()` call.
#'
#' @export
#'
#' @inheritParams base::sys.call
#' @inheritParams params
#'
#' @return
#' - `call`: `call`. Matched call.
#' - `list`: `list`. Verbose list that includes additional information about how
#'   the call was standardized.
#'
#' @seealso
#' - [base::match.call()].
#' - [base::sys.call()].
#' - [base::sys.parent()].
#'
#' @examples
#' aaa <- "AAA"
#' bbb <- "BBB"
#'
#' ## Standard function.
#' testing <- function(a, b) {
#'     standardizeCall()
#' }
#' testing(aaa, bbb)
#'
#' ## Inside S4 method.
#' setGeneric(
#'     name = "testing",
#'     def = function(a, b, ...) {
#'         standardGeneric("testing")
#'     }
#' )
#'
#' setMethod(
#'     f = "testing",
#'     signature = signature("character"),
#'     definition = function(a, b, ...) {
#'         standardizeCall()
#'     }
#' )
#' testing(aaa, bbb)
standardizeCall <- function(
    which = sys.parent(n = 1L),
    return = c("call", "list"),
    verbose = FALSE
) {
    assert_is_a_number(which)
    assert_all_are_non_negative(which)
    if (which < 1L) {
        which <- 1L
    }
    return <- match.arg(return)
    assert_is_a_bool(verbose)

    # Determine where the call is in the stack that we want to standardize.
    # Note that this differs for S4 methods containing a nested `.local()`.
    .local <- .isLocalCall(sys.call(which = which))
    if (isTRUE(.local) && which > 1L) {
        which <- which - 1L
    }

    # Local the parameters we need to sanitize call.
    definition <- sys.function(which = which)
    call <- sys.call(which = which)
    envir <- sys.frame(which = which)

    list <- list(
        sys.status = sys.status(),
        sys.nframe = sys.nframe(),
        sys.parent = sys.parent(),
        .local = .local,
        which = which,
        definition = definition,
        call = call,
        envir = envir
    )

    # Extract the definition from `.local()`, if necessary.
    if (isTRUE(.local)) {
        assert_that(!isTRUE(.isLocalCall(call)))
        # Update definition.
        if (is(definition, "MethodDefinition")) {
            # Pull the ".local()" function out, which has the formals we need to
            # match against in `match.call()` below.
            definition <- extractLocal(definition)
            list[["definition"]] <- definition
        }
    }

    if (isTRUE(verbose)) {
        print(list)
    }

    # Now ready to match (expand) the call.
    # @seealso `pryr::standardise_call()`.
    # Note that we need to use the `envir` argument to properly match.
    call <- match.call(
        definition = definition,
        call = call,
        expand.dots = TRUE,
        envir = envir
    )
    list[["match.call"]] <- call

    if (isTRUE(verbose)) {
        print(list(match.call = call))
    }

    # Check call integrity before returning.
    assert_is_call(call)

    # Require that all arguments are named before returning.
    # This check is especially important for S4 methods containing `.local()`.
    assert_all_are_non_missing_nor_empty_character(names(as.list(call)[-1L]))

    if (return == "list") {
        list
    } else {
        call
    }
}



.isLocalCall <- function(call) {
    assert_that(is(call, "call"))
    identical(call[[1L]], as.symbol(".local"))
}
