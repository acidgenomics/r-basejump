context("assertHasRownames")

data <- data.frame(
    sample1 = c(1L, 2L),
    sample2 = c(3L, 4L),
    row.names = c("gene1", "gene2"),
    stringsAsFactors = FALSE)
tibble <- tibble(
    sample1 = c(1L, 2L),
    sample2 = c(3L, 4L)
)

test_that("Success", {
    expect_silent(assertHasRownames(data))
})

test_that("Failure", {
    rownames(data) <- NULL
    expect_error(assertHasRownames(data))
    expect_error(assertHasRownames(tibble))
})
