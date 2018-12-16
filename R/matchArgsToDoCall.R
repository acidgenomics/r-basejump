#' Match arguments to `do.call`
#'
#' @inheritParams standardizeCall
#' @inheritParams base::do.call
#' @inheritParams params
#' @export
#'
#' @param removeFormals `character`.
#'   Names of formal arguments to remove from `args` list before passing to
#'   `do.call`.
#'
#' @return `list`.
#' Arguments to pass to `do.call`.
#'
#' @seealso
#' - `standardizeCall`.
#' - `do.call`.
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
    assert(
        isAny(args, classes = c("list", "NULL")),
        isAny(removeFormals, classes = c("character", "NULL")),
        isInt(which),
        isFlag(verbose)
    )

    if (is.list(args)) {
        assert(hasLength(args), hasNames(args))
    } else {
        args <- list()
    }

    if (which < 1L) {
        which <- 1L
    }

    list <- standardizeCall(
        which = which,
        return = "list",
        verbose = verbose
    )
    assert(is.list(list))
    definition <- list[["definition"]]
    assert(is.function(definition))
    call <- list[["match.call"]]
    assert(is.call(call))

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

    assert(
        hasNames(args),
        hasNoDuplicates(names(args))
    )
    invisible(lapply(
        X = args,
        FUN = function(x) {
            assert(!isAny(x, classes = c("call", "name", "symbol")))
        }
    ))

    args
}
