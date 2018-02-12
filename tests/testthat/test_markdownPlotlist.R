context("markdownPlotlist")

loadRemoteData("http://basejump.seq.cloud/plotlist.rda", quiet = TRUE)

test_that("List", {
    output <- capture.output(
        markdownPlotlist(plotlist)
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
