.onLoad <- function(libname, pkgname) {
    # Order is important here
    pkgs <-
        c("readxl",
          "stringr",
          "tidyverse",
          "rlang")
    lapply(seq_along(pkgs), function(a) {
        if (!pkgs[a] %in% (.packages())) {
            attachNamespace(pkgs[a])
        }
    })
    invisible()
}
