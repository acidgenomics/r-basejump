#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @import methods
#' @importFrom rlang abort inform warn
"_PACKAGE"



#' @importFrom utils globalVariables
globalVariables(".")
annotableCols <- c("ensgene", "symbol", "description", "biotype")
