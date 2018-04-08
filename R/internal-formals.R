.assignArgs <- function(
    envir = parent.frame()
) {
    x <- as.list(envir)
    for (i in seq_along(x)) {
        assign(
            x = names(x)[[i]],
            value = x[[i]],
            envir = envir
        )
    }
    x
}



.assignFormals <- function(
    fun = sys.function(sys.parent()),
    envir = parent.frame()
) {
    x <- formals(fun = fun)
    for (i in seq_along(x)) {
        assign(
            x = names(x)[[i]],
            value = x[[i]],
            envir = envir
        )
    }
    x
}
