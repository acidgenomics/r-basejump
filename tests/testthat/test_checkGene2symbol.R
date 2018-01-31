context("checkGene2symbol")

test_that("Success", {
    gene2symbol <- annotable(
        "Homo sapiens",
        format = "gene2symbol",
        quiet = TRUE)
    expect_true(checkGene2symbol(gene2symbol))
})

test_that("Failure", {
    expect_error(
        checkGene2symbol(mtcars),
        "gene2symbol must contain:"
    )
})

test_that("Object isn't a data.frame", {
    expect_error(
        checkGene2symbol(NULL),
        "gene2symbol must be a data.frame"
    )
})
