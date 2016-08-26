#' Assign multiple variables
#'
#' Optionally, you can specify a name prefix and the desired environment.
#'
#' @param ... \code{list} of variables to assign.
#' @param envir Desired \code{environment} (optional).
#' @param prefix Name prefix (optional).
#'
#' @return Assigns variables to new \code{environment} with a name prefix, if desired.
#' @export
multiassign <- function(..., envir = .GlobalEnv, prefix = NULL) {
  # The `-1` here removes `assign_multiple`
  names <- sapply(match.call(expand.dots = TRUE)[-1], deparse)
  data <- list(...)
  invisible(lapply(seq_along(data), function(x) {
    name <- names[x]
    if (!is.null(prefix)) {
      name <- paste(c(prefix, name), collapse = "_")
    }
    assign(name, data[[x]], envir = get(envir))
  }))
  ## ls.str(get(envir))
}
