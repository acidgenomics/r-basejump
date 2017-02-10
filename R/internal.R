utils::globalVariables(".")



#' @importFrom dplyr funs
funs <- function(...) {
    dplyr::funs(...)
}
