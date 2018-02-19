context("assertIsGene2symbol")

test_that("Success", {
    gene2symbol <- gene2symbol("Homo sapiens", quiet = TRUE)
    expect_silent(assertIsGene2symbol(gene2symbol))
})

test_that("Failure", {
    expect_error(
        assertIsGene2symbol(mtcars),
        paste(
            "are_identical :",
            "colnames\\(x\\) and c\\(\"ensgene\", \"symbol\"\\)",
            "are not identical."
        )
    )
    expect_error(
        assertIsGene2symbol(NULL),
        "is_data.frame : x"
    )
})
