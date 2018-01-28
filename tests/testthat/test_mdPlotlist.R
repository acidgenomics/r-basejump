context("mdPlotlist")

loadRemoteData("http://basejump.seq.cloud/plotlist.rda", quiet = TRUE)

test_that("Capture output", {
    output <- capture.output(
        mdPlotlist(plotlist)
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
