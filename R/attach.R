## Inspired by tidyverse approach:
## - https://github.com/tidyverse/tidyverse/blob/main/R/attach.R
## - https://github.com/tidyverse/tidyverse/blob/main/R/zzz.R



#' Core packages
#'
#' @note Updated 2023-05-17.
#' @noRd
.core <- c(
    "SummarizedExperiment",
    "SingleCellExperiment",
    "AcidBase",
    "syntactic",
    "pipette",
    "AcidPlyr",
    "AcidGenomes",
    "AcidExperiment",
    "AcidSingleCell",
    "AcidMarkdown"
)



#' Which core packages are unloaded, and not in the search path?
#'
#' @note Updated 2023-05-17.
#' @noRd
.coreUnloaded <- function() {
    .core[!paste0("package:", .core) %in% search()]
}



#' Attach the package from the same package library it was loaded from before
#'
#' @note Updated 2023-05-17.
#' @noRd
#'
#' @seealso
#' - https://github.com/tidyverse/tidyverse/issues/171
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



#' Code to run on package attachment
#'
#' @note Updated 2023-05-17.
#' @noRd
#'
#' @details
#' Note that `...` is necessary here, even though we're not using.
#'
#' The `suppressWarnings` wrapper is safe to remove once Bioconductor sorts out
#' some of the new NAMESPACE issues in the 3.17 update.
.onAttach <- function(...) {
    toLoad <- .coreUnloaded()
    suppressWarnings({
        suppressPackageStartupMessages({
            lapply(X = toLoad, FUN = .sameLibrary)
        })
    })
    invisible(toLoad)
}
