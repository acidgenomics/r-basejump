#' Header for object show method
#'
#' @note Updated 2019-08-11.
#' @export
#'
#' @inheritParams acidroxygen::params
#'
#' @return Console output, via [`cat()`][base::cat].
#'
#' @seealso [show()].
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
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
