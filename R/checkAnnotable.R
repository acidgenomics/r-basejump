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
    assert_is_data.frame(object)
    assert_is_subset(
        c("ensgene", "symbol", "description", "biotype", "broadClass"),
        colnames(object)
    )
    TRUE
}
