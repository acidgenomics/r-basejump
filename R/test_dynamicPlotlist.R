context("dynamicPlotlist")

loadRemoteData("http://basejump.seq.cloud/plotlist.rda")

test_that("Grid", {
    p <- dynamicPlotlist(plotlist, return = "grid")
    expect_is(p, "ggplot")
})

test_that("List", {
    list <- dynamicPlotlist(plotlist, return = "list")
    expect_is(list, "list")
    expect_identical(
        names(list),
        c("continuous", "discrete")
    )
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
