#' Check Ensembl Annotations
#'
#' @param object [data.frame] containing Ensembl gene annotations returned
#'   from the [annotable()] function.
#'
#' @return `TRUE` on pass, [stop()] on error.
#' @export
#'
#' @examples
#' annotable <- annotable("Homo sapiens")
#' checkAnnotable(annotable)
checkAnnotable <- function(object) {
    if (!is.data.frame(object)) {
        abort("Annotable must be a data.frame")
    }
    # Just check for the minimal set of required columns
    colnames <- c("ensgene", "symbol", "description", "biotype", "broadClass")
    if (!all(colnames %in% colnames(object))) {
        abort(paste(
            "Annotable must contain:", toString(colnames)
        ))
    }
    TRUE
}
