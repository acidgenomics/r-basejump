context("assertFormalAnnotationCol")

x <- data.frame(
    sample1 = c(1L, 2L),
    sample2 = c(3L, 4L),
    row.names = c("gene1", "gene2"),
    stringsAsFactors = FALSE)
colData <- data.frame(
    genotype = c("wt", "ko"),
    row.names = c("sample1", "sample2"),
    stringsAsFactors = TRUE)

test_that("Success", {
    expect_silent(assertFormalAnnotationCol(x, colData))
    expect_silent(assertFormalAnnotationCol(x, NA))
    expect_silent(assertFormalAnnotationCol(x, NULL))
})

test_that("Failure", {
    expect_error(assertFormalAnnotationCol(mtcars, colData))
})
