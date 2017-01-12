#' Assign multiple variables
#'
#' Optionally, you can specify a name prefix and the desired environment.
#'
#' @export
#' @param ... \code{list} of variables to assign.
#' @param envir Desired \code{environment} (optional).
#' @param prefix Name prefix (optional).
#' @return Assigns variables to new \code{environment} with a name prefix, if desired.
multiAssign <- function(..., envir = .GlobalEnv, prefix = NULL) {
    # The `-1` here removes the function name:
    names <- sapply(match.call(expand.dots = TRUE)[-1], deparse)
    data <- list(...)
    invisible(lapply(seq_along(data), function(x) {
        name <- names[x]
        if (!is.null(prefix)) {
            name <- paste(c(prefix, name), collapse = "_")
        }
        assign(name, data[[x]], envir = get(envir))
    }))
}
