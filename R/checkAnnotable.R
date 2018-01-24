#' Check Ensembl Annotations
#'
#' @param object [data.frame] containing Ensembl gene annotations returned
#'   from the [annotable()] function.
#'
#' @return Silent on pass, stop on error.
#' @export
#'
#' @examples
#' # Success
#' annotable <- annotable("Homo sapiens")
#' checkAnnotable(annotable)
#'
#' # Failure
#' \dontrun{
#' checkAnnotable(mtcars)
#' }
checkAnnotable <- function(object) {
    if (!is.data.frame(object)) {
        abort("annotable must be data.frame")
    }
    # Just check for the minimal set of required columns
    colnames <- c("ensgene", "symbol", "description", "biotype", "broadClass")
    if (!all(colnames %in% colnames(object))) {
        abort(paste(
            "annotable must contain:", toString(colnames)
        ))
    }
}
