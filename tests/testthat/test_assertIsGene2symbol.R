context("assertIsGene2symbol")

test_that("Success", {
    gene2symbol <- annotable(
        "Homo sapiens",
        format = "gene2symbol",
        quiet = TRUE)
    expect_silent(assertIsGene2symbol(gene2symbol))
})

test_that("Failure", {
    expect_error(
        assertIsGene2symbol(mtcars),
        "are_identical"
    )
})

test_that("Object isn't a data.frame", {
    expect_error(
        assertIsGene2symbol(NULL),
        "is_data.frame"
    )
})
