# FIXME Improve documentation

# call = match.call
# fun = sys.function(sys.parent())



#' Set Arguments to Do Call
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @return `list`. Arguments to pass to [do.call()].
setArgsToDoCall <- function(
    args,
    removeArgs = NULL,
    call,
    fun
) {
    assert_is_list(args)
    assert_has_names(args)
    assert_is_any_of(removeArgs, c("character", "NULL"))
    assert_is_call(call)
    assert_is_function(fun)
    # FIXME Improve the assert checks to check the names passed in here.

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

    stopifnot(!any(duplicated(names(args))))
    args
}
