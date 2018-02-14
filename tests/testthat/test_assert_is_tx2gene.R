context("assert_is_tx2gene")

test_that("Success", {
    tx2gene <- annotable(
        "Homo sapiens",
        format = "tx2gene",
        quiet = TRUE)
    expect_silent(assert_is_tx2gene(tx2gene))
})

test_that("Failure", {
    expect_error(
        assert_is_tx2gene(mtcars),
        "are_identical"
    )
})

test_that("Object isn't a data.frame", {
    expect_error(
        assert_is_tx2gene(NULL),
        "is_data.frame"
    )
})
