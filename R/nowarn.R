nowarn <- function() {
    assign("last.warning", NULL, envir = baseenv())
}
