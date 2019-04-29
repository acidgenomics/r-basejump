#' Header for object show method
#'
#' @inheritParams params
#'
#' @return Console output, via [`cat()`][base::cat].
#'
#' @seealso [methods::show()].
#'
#' @examples
#' data(rse, package = "acidtest")
#' showHeader(rse)
showHeader <- function(object) {
    class <- class(object)[[1L]]
    out <- class

    version <- as.character(metadata(object)[["version"]])
    if (hasLength(version)) {
        out <- paste(out, version)
    }

    length <- length(object)
    if (!is.null(length)) {
        out <- paste(out, "of length", length)
    }

    cat(out, "\n", sep = "")
}
