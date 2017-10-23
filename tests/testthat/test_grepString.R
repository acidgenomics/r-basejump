context("grepString")

test_that("grepString", {
    expect_equal(
        grepString("gene"),
        "^gene$|^gene,|\\sgene,|\\sgene$"
    )
})
