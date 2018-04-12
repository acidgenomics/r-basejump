# Utility functions for providing snake case alias support

.assignCamelArgs <- function(
    envir = parent.frame()
) {
    x <- as.list(envir)
    x <- camel(x)
    for (i in seq_along(x)) {
        assign(
            x = names(x)[[i]],
            value = x[[i]],
            envir = envir
        )
    }
    x
}

.assignCamelFormals <- function(
    fun = sys.function(sys.parent()),
    envir = parent.frame()
) {
    x <- formals(fun = fun)
    x <- camel(x)
    for (i in seq_along(x)) {
        assign(
            x = names(x)[[i]],
            value = x[[i]],
            envir = envir
        )
    }
    x
}
