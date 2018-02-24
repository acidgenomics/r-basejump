context("sanitizeColData")

test_that("sanitizeColData", {
    x <- coldata
    x[["day"]] <- c(14L, 14L, 30L, 30L)
    x <- sanitizeColData(x)
    expect_is(x, "DataFrame")
    expect_identical(rownames(x), rownames(coldata))
    expect_true(all(vapply(
        X = x,
        FUN = is.factor,
        FUN.VALUE = logical(1L))))
})
