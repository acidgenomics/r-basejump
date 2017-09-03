context("Package Utilities")

test_that("deprecated", {
    expect_warning(
        packageSE(),
        "'packageSE' is deprecated.")
})



test_that("onLoad",  {
    expect_equal(
        loadNamespace("basejump") %>%
            class,
        "environment")
    expect_error(
        attachNamespace("annotables"),
        "namespace is already attached")
})
