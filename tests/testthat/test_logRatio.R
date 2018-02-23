context("logRatio")

test_that("foldChangeToLogRatio", {
    expect_equal(foldChangeToLogRatio(fc), lr)
    expect_error(
        foldChangeToLogRatio(lr, base = 0L),
        "is_positive : base"
    )
})

test_that("logRatioToFoldChange", {
    expect_equal(logRatioToFoldChange(lr), fc)
    expect_error(
        logRatioToFoldChange(lr, base = 0L),
        "is_positive : base"
    )
})


