#' Minimal Sample Data
#'
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
    assertCharacter(sample)
    assert_has_no_duplicates(sample)
    sample <- as.factor(sample)
    DataFrame(sampleName = sample, row.names = makeNames(sample))
}
