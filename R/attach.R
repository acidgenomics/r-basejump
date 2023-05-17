## Inspired by tidyverse approach:
## - https://github.com/tidyverse/tidyverse/blob/main/R/attach.R
## - https://github.com/tidyverse/tidyverse/blob/main/R/zzz.R



#' Core packages
#'
#' @note Updated 2023-05-17.
#' @noRd
.corePkgs <- c(
    "magrittr",
    ## BiocGenerics
    ## AcidGenerics
    ## stats
    ## GenomicRanges
    ## IRanges
    ## S4Vectors
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
    pkgs <- .corePkgs
    suppressWarnings({
        suppressPackageStartupMessages({
            lapply(
                X = pkgs,
                FUN = library,
                character.only = TRUE,
                warn.conflicts = FALSE
            )
        })
    })
    invisible(pkgs)
}
