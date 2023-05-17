## Inspired by tidyverse approach:
## - https://github.com/tidyverse/tidyverse/blob/main/R/attach.R
## - https://github.com/tidyverse/tidyverse/blob/main/R/zzz.R



#' Core packages
#'
#' @note Updated 2023-05-17.
#' @noRd
.corePkgs <- c(
    ## CRAN ----
    "magrittr",
    ## Bioconductor ----
    "SummarizedExperiment",
    "SingleCellExperiment",
    ## Acid Genomics ----
    "AcidBase",
    "AcidExperiment",
    "AcidGenomes",
    "AcidMarkdown",
    "AcidPlots",
    "AcidPlyr",
    "AcidSingleCell",
    "pipette",
    "syntactic"
)



#' Code to run on package attachment
#'
#' @note Updated 2023-05-17.
#' @noRd
#'
#' @details
#' The `suppressWarnings` wrapper is safe to remove once Bioconductor sorts out
#' some of the new NAMESPACE issues in the 3.17 update.
.onAttach <- function(...) { # nolint
    pkgs <- .corePkgs
    suppressWarnings({
        suppressPackageStartupMessages({
            lapply(
                X = pkgs,
                FUN = library, # nolint
                character.only = TRUE,
                warn.conflicts = FALSE
            )
        })
    })
    invisible(pkgs)
}
