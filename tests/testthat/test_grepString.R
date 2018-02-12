context("grepString")

test_that("grepString", {
    expect_identical(
        grepString("gene"),
        "^gene$|^gene,|\\sgene,|\\sgene$"
    )
})
