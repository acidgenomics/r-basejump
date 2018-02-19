context("assertIsTx2gene")

test_that("Success", {
    tx2gene <- tx2gene("Homo sapiens", quiet = TRUE)
    expect_silent(assertIsTx2gene(tx2gene))
})

test_that("Failure", {
    expect_error(
        assertIsTx2gene(mtcars),
        paste(
            "are_identical :",
            "colnames\\(x\\) and c\\(\"enstxp\", \"ensgene\"\\)",
            "are not identical."
        )
    )
    expect_error(
        assertIsTx2gene(NULL),
        "is_data.frame : x"
    )
})
