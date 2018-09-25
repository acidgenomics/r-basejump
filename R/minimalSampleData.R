#' Minimal Sample Data
#'
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @param sample `character`. Sample names (e.g. per-sample directory names).
#'
#' @return `DataFrame`.
#'
#' @examples
#' x <- minimalSampleData(sample = c("sample 1", "sample 2"))
#' print(x)
minimalSampleData <- function(sample) {
    assert_is_character(sample)
    assert_has_no_duplicates(sample)
    sample <- as.factor(sample)
    DataFrame(
        sampleName = sample,
        row.names = makeNames(sample)
    )
}
