assign_multiple <- function(..., envir = .GlobalEnv, prefix = NULL) {
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
  ls.str(get(envir))
}
