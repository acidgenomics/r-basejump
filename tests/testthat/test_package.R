context("Package Utilities")

test_that("onLoad", {
    expect_error(
        attachNamespace("annotables"),
        "namespace is already attached")
})
