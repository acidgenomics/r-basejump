#' Check Ensembl Annotations
#'
#' @param object [data.frame] containing Ensembl gene annotations returned
#'   from the [annotable()] function.
#'
#' @return Silent on pass, stop on error.
#' @export
#'
#' @examples
#' checkAnnotable(mtcars)
checkAnnotable <- function(object) {
    if (!is.data.frame(object)) {
        stop("annotable must be 'data.frame' class object",
             call. = FALSE)
    }
    # Just check for the minimal set of required columns
    colnames <- c("ensgene", "symbol", "description", "biotype", "broadClass")
    if (!all(colnames %in% colnames(object))) {
        stop(paste(
            "annotable must contain:", toString(colnames)
        ), call. = FALSE)
    }
}
