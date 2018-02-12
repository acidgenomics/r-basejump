#' Check Transcript to Gene Mapping Data
#'
#' @param object [data.frame] containing Ensembl transcript to gene identifier
#'   mappings. Must be structured as a two column [data.frame] with "enstxp" and
#'   "ensgene" columns.
#'
#' @return Invisible `TRUE` on pass, [stop()] on error.
#' @export
#'
#' @examples
#' tx2gene <- annotable("Homo sapiens", format = "tx2gene")
#' assert_is_tx2gene(tx2gene)
assert_is_tx2gene <- function(object) {
    assert_is_data.frame(object)
    assert_are_identical(
        colnames(object),
        c("enstxp", "ensgene")
    )
}
