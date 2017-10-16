#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @import methods
"_PACKAGE"

globalVariables(".")

metadataPriorityCols <- c("sampleID", "sampleName", "description")

# Use this for `data-raw/` scripts to define output path
testDataDir <- file.path("docs", "tests")

#' Test Data URL
#' @keywords internal
#' @export
testDataURL <- file.path(
    "https://raw.githubusercontent.com",
    "steinbaugh",
    "basejump",
    "develop",
    "docs",
    "tests")

#' Package Website URL
#' @keywords internal
#' @export
url <- "http://steinbaugh.com/basejump"
