.assignFormals <- function(
    fun = sys.function(sys.parent())
) {
    f <- formals(fun = fun)
    for (i in seq_along(f)) {
        assign(x = names(f)[[i]], value = f[[i]])
    }
}
