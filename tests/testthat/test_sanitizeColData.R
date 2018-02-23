context("sanitizeColData")

test_that("DataFrame", {
    data <- sanitizeColData(colData)
    expect_true(all(vapply(
        X = data,
        FUN = is.factor,
        FUN.VALUE = logical(1L))))
    expect_identical(
        data,
        DataFrame(
            row.names = rownames,
            genotype = as.factor(genotype),
            batch = as.factor(batch)
        )
    )
})
