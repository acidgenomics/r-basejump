#' Assert Is GFF
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @param x GFF `data.frame`.
#'
#' @export
assertIsGFF <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_data.frame(x, severity = severity)
    assert_is_of_length(
        x = colnames(x),
        n = 9L,
        severity = severity
    )
    assert_are_identical(
        x = colnames(x),
        y = gffCols,
        severity = severity
    )
}
