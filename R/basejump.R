#' basejump package.
#'
#' Base functions for bioinformatics and R package development.
#'
#' Consult the \href{http://steinbaugh.com/basejump/}{package website} for more
#' information.
#'
#' @docType package
#' @name basejump
#' @keywords internal
NULL



# Globals ====
globalVariables(c(".", "biocLite"))



# Imports ====
# General ----
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom RCurl getURL
#' @importFrom rmarkdown render
#' @importFrom R.utils gzip
#' @importFrom stats na.omit setNames
#' @importFrom utils download.file globalVariables
NULL

# Development ----
#' @import devtools
#' @importFrom BiocCheck BiocCheck
#' @importFrom BiocInstaller biocValid
NULL

# tidyverse ----
# http://tidyverse.org/
#' @import dplyr
#' @import readr
#' @import magrittr
#' @import stringr
#' @import tibble
NULL



# Re-exports ====
#' @usage NULL
#' @export
magrittr::`%>%`
