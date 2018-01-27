#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @import methods
#' @importFrom rlang !! !!! .data abort dots_list eval_bare inform is_string sym
#'   syms warn
"_PACKAGE"

#' @importFrom utils globalVariables
globalVariables(".")

metadataPriorityCols <- c("sampleID", "sampleName", "description")
