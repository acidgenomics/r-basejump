.getMakeNamesFunction <- function(x) {
    .assertFormalMakeNames(x)
    get(x = x, envir = asNamespace("basejump"), inherits = FALSE)
}
