context("cell2sample")

test_that("SCE factor return", {
    x <- cell2sample(sce, return = "factor")
    expect_is(x, "factor")
    expect_identical(levels(x), c("sample1", "sample2"))
    expect_identical(
        object = head(names(x)),
        expected = paste0("cell00", seq_len(6L))
    )
})

test_that("SCE tibble return", {
    x <- cell2sample(sce, return = "tibble")
    expect_s3_class(x, "tbl_df")
    expect_identical(colnames(x), c("cellID", "sampleID"))
})
