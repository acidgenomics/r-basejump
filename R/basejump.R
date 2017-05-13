#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @import dplyr
#' @import magrittr
#' @import readr
#' @import stringr
#' @import tibble
#' @importFrom BiocCheck BiocCheck
#' @importFrom BiocInstaller biocValid
#' @importFrom devtools build build_vignettes check document install load_all
#'   test
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom R.utils gzip
#' @importFrom RCurl getURL
#' @importFrom rlang is_string
#' @importFrom rmarkdown render
#' @importFrom stats na.omit setNames
#' @importFrom utils download.file globalVariables
"_PACKAGE"



# Globals ====
globalVariables(c(".", "biocLite"))



# Re-exports ====
#' @usage NULL
#' @export
magrittr::`%>%`

#' @usage NULL
#' @export
rlang::is_string

#' @usage NULL
#' @export
magrittr::set_colnames

#' @usage NULL
#' @export
magrittr::set_names

#' @usage NULL
#' @export
magrittr::set_rownames
