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
    assert_that(is.data.frame(object))
    # Just check for the minimal set of required columns
    requiredCols <- c(
        "ensgene",
        "symbol",
        "description",
        "biotype",
        "broadClass"
    )
    if (!all(requiredCols %in% colnames(object))) {
        abort(paste(
            paste0("`", deparse(substitute(object)), "`"),
            "must contain:", toString(requiredCols)
        ))
    }
    TRUE
}
