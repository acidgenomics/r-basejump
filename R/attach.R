## Updated 2023-05-17.
.core <- c(
    "AcidBase",
    "syntactic",
    "pipette",
    "AcidPlyr",
    "AcidGenomes",
    "AcidExperiment",
    "AcidSingleCell",
    "AcidMarkdown"
)



## Updated 2023-05-17.
.coreUnloaded <- function() {
    search <- paste0("package:", .core)
    .core[!search %in% search()]
}



# Attach the package from the same package library it was
# loaded from before. https://github.com/tidyverse/tidyverse/issues/171
.sameLibrary <- function(pkg) {
    loc <- if (pkg %in% loadedNamespaces()) {
        dirname(getNamespaceInfo(pkg, "path"))
    }
    library(
        package = pkg,
        lib.loc = loc,
        character.only = TRUE,
        warn.conflicts = FALSE
    )
}



## Updated 2023-05-17.
.acidAttach <- function() {
    toLoad <- .coreUnloaded()
    suppressPackageStartupMessages({
        lapply(X = toLoad, FUN = .sameLibrary)
    })
    invisible(toLoad)
}



## Updated 2023-05-17.
.onAttach <- function(...) {
    .acidAttach()
}
