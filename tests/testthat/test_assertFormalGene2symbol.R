context("assertFormalGene2symbol")

test_that("Success", {
    expect_silent(assertFormalGene2symbol(x, genes, gene2symbol))
})

test_that("Failure", {
    expect_error(
        assertFormalGene2symbol(mtcars, genes, gene2symbol),
        paste(
            "is_subset :",
            "The elements 'ENSG00000000003', 'ENSG00000000005'",
            "in genes are not in rownames\\(x\\)."
        )
    )
})
