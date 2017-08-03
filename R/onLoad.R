.onLoad <- function(libname, pkgname) {
    pkgs <- c("annotables",
              "ggplot2",
              "SummarizedExperiment")
    lapply(seq_along(pkgs), function(a) {
        if (!pkgs[[a]] %in% (.packages())) {
            attachNamespace(pkgs[[a]])
        }
    })
    invisible()
}
