#' basejump
#'
#' Base functions for bioinformatics and R package development.
"_PACKAGE"



#' Global variables.
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
