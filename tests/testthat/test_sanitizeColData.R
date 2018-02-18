context("sanitizeColData")

rownames <- c("sample_1", "sample_2", "sample_3", "sample_4")
genotype <- c("wt", "ko", "wt", "ko")
batch <- c(1L, 1L, 2L, 2L)
colData <- DataFrame(
    row.names = rownames,
    genotype = genotype,
    batch = batch
)

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
