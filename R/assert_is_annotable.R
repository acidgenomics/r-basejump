#' Ensembl Annotations Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @param x [data.frame] containing Ensembl gene annotations returned
#'   from the [annotable()] function.
#'
#' @export
#'
#' @examples
#' annotable <- annotable("Homo sapiens")
#' assert_is_annotable(annotable)
assert_is_annotable <- function(x) {  # nolint
    assert_is_data.frame(x)
    assert_is_subset(
        c("ensgene", "symbol", "description", "biotype", "broadClass"),
        colnames(x)
    )
}
