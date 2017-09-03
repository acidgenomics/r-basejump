.onLoad <- function(libname, pkgname) {
    pkgs <- "annotables"
    lapply(seq_along(pkgs), function(a) {
        if (!pkgs[a] %in% (.packages())) {
            attachNamespace(pkgs[a])
        }
    })
    invisible()
}
