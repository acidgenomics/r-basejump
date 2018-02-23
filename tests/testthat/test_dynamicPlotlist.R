context("dynamicPlotlist")

test_that("Grid", {
    p <- dynamicPlotlist(plotlist, return = "grid")
    expect_s3_class(p, "ggplot")
})

test_that("List", {
    list <- dynamicPlotlist(plotlist, return = "list")
    expect_is(list, "list")
})

test_that("Markdown", {
    output <- capture.output(
        dynamicPlotlist(plotlist, return = "markdown")
    )
    expect_identical(
        output,
        c(
            "",
            "",
            "## continuous",
            "",
            "",
            "",
            "## discrete",
            ""
        )
    )
})
