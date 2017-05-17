#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' The package exports [camel()] and [snake()] variants of all functions,
#' for seamless integration into [Bioconductor](https://www.bioconductor.org)
#' and [tidyverse](http://tidyverse.org) workflows. Function parameters are
#' always passed in [camel()] case. We also provide British spelling variants
#' (e.g. colour) of the function names wherever applicable.
#'
#' @importFrom yaml yaml.load_file
"_PACKAGE"



#' Global variables
#'
#' @keywords internal
#' @export
#'
#' @seealso
#' - [globalVariables()].
#' - [import_tidy_verbs()].
#' - [reexports].
globals <- c(".",
             "arrange",
             "biocLite",
             "filter",
             "mutate",
             "select")
globalVariables(globals)
