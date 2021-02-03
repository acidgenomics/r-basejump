context("coerceToList")

test_that("coerceToList", {
    object <- coerceToList(rse)
    expect_is(object, "list")
    expect_identical(
        object = names(object),
        expected = c(
            "rowRanges",
            "colData",
            "assays",
            "NAMES",
            "elementMetadata",
            "metadata"
        )
    )
})
