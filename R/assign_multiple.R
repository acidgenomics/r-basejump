assign_multiple <- function(vec, envir = ".GlobalEnv", prefix = NULL) {
  invisible(lapply(seq_along(vec), function(x) {
    if (is.null(prefix)) {
      name <- vec[x]
    } else {
      name <- paste0(c(prefix, vec[x]), collapse = "_")
    }
    assign(name, get(vec[x]), envir = get(envir))
  }))
  print(envir)
  ls.str(get(envir))
}
