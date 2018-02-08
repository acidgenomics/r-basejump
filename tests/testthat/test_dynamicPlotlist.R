context("dynamicPlotlist")

loadRemoteData("http://basejump.seq.cloud/plotlist.rda", quiet = TRUE)

test_that("grid", {
    p <- dynamicPlotlist(plotlist, return = "grid")
    expect_s3_class(p, "ggplot")
})

test_that("list", {
    list <- dynamicPlotlist(plotlist, return = "list")
    expect_is(list, "list")
})

test_that("markdown", {
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
