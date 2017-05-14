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
#' - [reexports].
#' - [tidy_verbs()].
globals <- c(".",
             "arrange",
             "biocLite",
             "filter",
             "mutate",
             "select")
globalVariables(globals)
