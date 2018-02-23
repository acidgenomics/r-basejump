context("sanitizeColData")

test_that("DataFrame", {
    x <- sanitizeColData(coldata)
    expect_is(x, "DataFrame")
    expect_identical(dimnames(x), dimnames(coldata))
    expect_true(all(vapply(
        X = x,
        FUN = is.factor,
        FUN.VALUE = logical(1L))))
})
