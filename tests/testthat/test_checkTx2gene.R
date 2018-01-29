context("checkTx2gene")

test_that("Success", {
    tx2gene <- annotable(
        "Homo sapiens",
        format = "tx2gene",
        quiet = TRUE)
    expect_true(checkTx2gene(tx2gene))
})

test_that("Failure", {
    expect_error(
        checkTx2gene(mtcars),
        "tx2gene must contain:"
    )
})
