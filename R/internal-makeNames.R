.getMakeNamesFunction <- function(x) {
    get(x = x, envir = asNamespace("basejump"), inherits = FALSE)
}
