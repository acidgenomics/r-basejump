context("assertIsTx2gene")

test_that("Success", {
    tx2gene <- annotable(
        "Homo sapiens",
        format = "tx2gene",
        quiet = TRUE)
    expect_silent(assertIsTx2gene(tx2gene))
})

test_that("Failure", {
    expect_error(
        assertIsTx2gene(mtcars),
        "are_identical"
    )
})

test_that("Object isn't a data.frame", {
    expect_error(
        assertIsTx2gene(NULL),
        "is_data.frame"
    )
})
