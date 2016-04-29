assign_multiple <- function(..., envir = .GlobalEnv, prefix = NULL) {
  list <- list(...)
  ##### switch to other option from lapply here...
    invisible(lapply(seq_along(list), function(x) {
    name <- deparse(substitute(list[[x]]))
    if (!is.null(prefix)) {
      name <- paste(c(prefix, name), collapse = "_")
    }
    assign(name, list[[x]], envir = get(envir))
  }))
}
