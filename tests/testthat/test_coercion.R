context("Coercion")

data(rse, envir = environment())



# coerceS4ToList ===============================================================
test_that("coerceS4ToList", {
    object <- coerceS4ToList(rse)
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



# tibble =======================================================================
test_that("tibble", {
    data <- SummarizedExperiment::colData(rse)
    x <- as(data, "tbl_df")
    expect_is(x, "tbl_df")

    # Expect that rownames are automatically moved to first column.
    expect_identical(colnames(x)[[1L]], "rowname")

    # Early return if already tibble.
    x <- tibble::tibble()
    expect_identical(x, as(x, "tbl_df"))
})
